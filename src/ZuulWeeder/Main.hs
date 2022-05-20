module ZuulWeeder.Main (main, mainWithArgs, runDemo, demoConfig) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Data.ByteString qualified as BS
import Data.IORef
import Data.Maybe (isNothing)
import Data.Text qualified as Text
import Data.Yaml (decodeThrow)
import Main.Utf8 (withUtf8)
import Network.HTTP.Types.Status qualified as HTTP
import Network.Socket qualified
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp as Warp (run)
import Prometheus qualified
import Prometheus.Metric.GHC qualified
import Streaming
import Streaming.Prelude qualified as S
import System.Environment
import System.Log.FastLogger qualified
import System.Timeout (timeout)
import Web.HttpApiData (toHeader)
import Zuul.ConfigLoader (Config (..), TenantResolver, UrlBuilder, emptyConfig, loadConfig)
import Zuul.ServiceConfig (ServiceConfig (..), readServiceConfig)
import Zuul.Tenant
import Zuul.ZooKeeper
import ZuulWeeder.Graph
import ZuulWeeder.Prelude
import ZuulWeeder.UI qualified

data Args = Args
  { zkPath :: FilePathT,
    configPath :: FilePathT
  }

getEnvArgs :: IO Args
getEnvArgs =
  Args <$> envPath "WEEDER_DATA" "/var/tmp/weeder" <*> envPath "ZUUL_CONF" "/etc/zuul/zuul.conf"
  where
    envPath :: String -> FilePath -> IO FilePathT
    envPath name def = FilePathT . Text.pack . fromMaybe def <$> lookupEnv name

main :: IO ()
main = withUtf8 $ withLogger (\l -> getEnvArgs >>= mainWithArgs l)

withLogger :: (Logger -> IO ()) -> IO ()
withLogger cb = do
  tc <- System.Log.FastLogger.newTimeCache "%F %T "
  System.Log.FastLogger.withTimedFastLogger tc l cb
  where
    l = System.Log.FastLogger.LogStderr 1024

mainWithArgs :: Logger -> Args -> IO ()
mainWithArgs logger args = do
  (configDump, configLoader) <- do
    configLoader <- runExceptT $ mkConfigLoader logger args.zkPath args.configPath
    case configLoader of
      Left e -> error $ Text.unpack $ "Can't load config: " <> e
      Right x -> pure x
  config <- configReloader logger configDump configLoader
  runWeb logger config

runDemo :: IO ()
runDemo = do
  Prometheus.unregisterAll
  withLogger $ \l -> runWeb l demoConfig

runWeb :: Logger -> IO Analysis -> IO ()
runWeb logger config = do
  rootUrl <- ensureTrailingSlash . Text.pack . fromMaybe "/" <$> lookupEnv "WEEDER_ROOT_URL"
  distPath <- fromMaybe "dists" <$> lookupEnv "WEEDER_DIST_PATH"
  port <- maybe 9001 read <$> lookupEnv "WEEDER_PORT"
  info logger $ "[+] serving 0.0.0.0:" <> toHeader port <> from rootUrl
  let app = ZuulWeeder.UI.app config (ZuulWeeder.UI.RootURL rootUrl) distPath
  -- monitornig
  void $ Prometheus.register Prometheus.Metric.GHC.ghcMetrics
  counter <- Prometheus.register $ Prometheus.counter (Prometheus.Info "http_request" "")
  Warp.run port $ monitoring logger counter app
  where
    ensureTrailingSlash url = case Text.unsnoc url of
      Nothing -> "/"
      Just (x, '/') -> ensureTrailingSlash x
      _ -> Text.snoc url '/'

monitoring :: Logger -> Prometheus.Counter -> Wai.Middleware
monitoring logger counter baseApp req resp = case Wai.rawPathInfo req of
  "/health" -> resp $ Wai.responseLBS HTTP.ok200 [] mempty
  "/metrics" -> resp . Wai.responseLBS HTTP.ok200 [] =<< Prometheus.exportMetricsAsText
  p | "/dists/" `BS.isPrefixOf` p -> baseApp req resp
  p -> do
    measure <- intervalMilliSec
    baseApp req $ \r -> do
      Prometheus.incCounter counter
      result <- resp r
      elapsed <- measure
      let statusCode = HTTP.statusCode $ Wai.responseStatus r
          htmx = any (\h -> fst h == "HX-Request") $ Wai.requestHeaders req
          client = remoteHash (Wai.remoteHost req) + hash (Wai.requestHeaderUserAgent req)
          msg =
            p
              <> (" code=" <> toHeader statusCode)
              <> (" ms=" <> toHeader elapsed)
              <> (" htmx=" <> toHeader htmx)
              <> (" client=" <> toHeader client)
              <> "\n"
      info logger msg
      pure result
  where
    remoteHash :: Network.Socket.SockAddr -> Int
    remoteHash = \case
      Network.Socket.SockAddrInet _ h -> hash h
      Network.Socket.SockAddrInet6 _ h _ _ -> hash h
      Network.Socket.SockAddrUnix _ -> 0

newtype ConfigDumper = ConfigDumper {dumpConfig :: ExceptT Text IO ()}

newtype ConfigLoader = ConfigLoader {loadConfig :: ExceptT Text IO (TenantsConfig, Config)}

-- | Create a IO action that reloads the config every hour.
configReloader :: Logger -> ConfigDumper -> ConfigLoader -> IO (IO Analysis)
configReloader logger configDumper configLoader = do
  -- Get current time
  now <- getSec
  -- Read the inital conf, error is fatal here
  conf <- either (error . Text.unpack) id <$> runExceptT (configLoader.loadConfig)
  -- Cache the result
  cache <- newIORef (uncurry analyzeConfig conf)
  ts <- newMVar (now, cache)
  pure (modifyMVar ts go)
  where
    go :: (Int64, IORef Analysis) -> IO ((Int64, IORef Analysis), Analysis)
    go (ts, cache) = do
      analysis <- readIORef cache
      now <- getSec
      if now - ts < 3600
        then pure ((ts, cache), analysis)
        else do
          reload cache
          pure ((now, cache), analysis)

    reload :: IORef Analysis -> IO ()
    reload cache = void $ forkIO do
      res <- timeout 600_000_000 $ do
        info logger "ReLoading the configuration"
        confE <- runExceptT do
          configDumper.dumpConfig
          configLoader.loadConfig
        case confE of
          Left e -> do
            info logger ("Error reloading config: " <> from e)
          Right conf -> do
            info logger "Caching the graph result"
            writeIORef cache (uncurry analyzeConfig conf)
      when (isNothing res) do
        info logger "Error reloading config timeout"

-- | Create IO actions to dump and load the config
mkConfigLoader :: Logger -> FilePathT -> FilePathT -> ExceptT Text IO (ConfigDumper, ConfigLoader)
mkConfigLoader logger dataBaseDir configFile = do
  -- Load the zuul.conf
  serviceConfig <- readServiceConfig (readFileText configFile)
  pure (configDumper serviceConfig, go serviceConfig)
  where
    dataDir = dataBaseDir </> "data"
    configDumper :: ServiceConfig -> ConfigDumper
    configDumper serviceConfig = ConfigDumper do
      env <- lift $ lookupEnv "ZUUL_WEEDER_NO_ZK"
      case env of
        Just _ -> lift $ hPutStrLn stderr "[+] ZUUL_WEEDER_NO_ZK is set, skipping dumpZK"
        Nothing -> dumpZKConfig logger dataDir serviceConfig.zookeeper
    cp = dataDir </> FilePathT "zuul/system/conf/0000000000"
    go :: ServiceConfig -> ConfigLoader
    go serviceConfig = ConfigLoader do
      -- ensure data-dir exists
      whenM (not <$> lift (doesDirectoryExist cp)) (dumpConfig $ configDumper serviceConfig)
      -- read the tenants config from dataDir
      systemConfig <- readSystemConfig dataDir
      -- decode the tenants config
      tenantsConfig <- except (decodeTenantsConfig systemConfig `orDie` "Invalid tenant config")
      -- load all the config objects
      let tr = Zuul.Tenant.tenantResolver serviceConfig tenantsConfig
      config <- lift $ loadConfigFiles serviceConfig.urlBuilders tr dataDir
      pure (tenantsConfig, config)

loadConfigFiles :: UrlBuilder -> TenantResolver -> FilePathT -> IO Zuul.ConfigLoader.Config
loadConfigFiles ub tr =
  flip execStateT Zuul.ConfigLoader.emptyConfig
    -- StateT Config IO ()
    . S.effects
    -- Apply the loadConfig function to each element
    . S.chain (Zuul.ConfigLoader.loadConfig ub tr)
    -- Stream (Of ZKConfig) (StateT Config IO)
    . hoist lift
    -- Stream (Of ZKConfig) IO
    . walkConfigNodes

demoConfig :: IO Analysis
demoConfig = do
  (tenantsConfig, config) <-
    either (error . show) id
      <$> runExceptT do
        serviceConfig <-
          readServiceConfig
            ( pure
                [s|
[zookeeper]
hosts=localhost
tls_cert=cert.pem
tls_key=key.pem
tls_ca=ca.pem

[connection gerrit]
driver=gerrit
server=managesf.sftests.com
canonical_hostname=sftests.com

[connection git]
driver=git
baseurl=http://localhost/cgit
|]
            )
        systemConfig <-
          ZKSystemConfig
            <$> decodeThrow
              [s|
unparsed_abide:
  tenants:
    demo:
      source:
        git:
          config-projects:
            - project-config: {}
    local:
      source:
        gerrit:
          config-projects:
            - config: {}
          untrusted-projects:
            - sf-jobs: {}
            - zuul-jobs:
                include: [job]
                shadow: sf-jobs
|]
        tenantsConfig <- except (decodeTenantsConfig systemConfig `orDie` "Invalid tenant config")
        let tr = Zuul.Tenant.tenantResolver serviceConfig tenantsConfig
        conf <- lift $ flip execStateT Zuul.ConfigLoader.emptyConfig do
          xs <- sequence configFiles
          traverse_ (Zuul.ConfigLoader.loadConfig serviceConfig.urlBuilders tr) (pure <$> xs)
        pure (tenantsConfig, conf)

  pure $ analyzeConfig tenantsConfig config
  where
    mkConfigFile conn proj conf =
      ZKConfig conn proj "main" (FilePathT ".zuul.yaml") (FilePathT "/") <$> decodeThrow conf
    configFiles =
      [ mkConfigFile
          "sftests.com"
          "config"
          [s|
- job:
    name: base
    nodeset: centos

- nodeset:
    name: centos
    nodes:
      - name: runner
        label: cloud-centos-7
|],
        mkConfigFile
          "sftests.com"
          "sf-jobs"
          [s|
- job:
    name: linter
    nodeset:
      nodes:
        - name: container
          label: pod-centos-7
|],
        mkConfigFile
          "localhost"
          "project-config"
          [s|
- job:
    name: base
    nodeset: rhel

- nodeset:
    name: rhel
    nodes:
      - name: runner
        label: cloud-rhel-7
|]
      ]
