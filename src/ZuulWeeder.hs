-- |
-- Module      : ZuulWeeder
-- Description : The project entrypoint
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
--
-- Maintainer  : tdecacqu@redhat.com, fboucher@redhat.com
-- Stability   : provisional
-- Portability : portable
--
-- The project entrypoint.
module ZuulWeeder (main, runDemo, demoConfig) where

import Data.Text qualified as Text
import Data.Yaml (decodeThrow)
import Network.Wai.Handler.Warp as Warp (run)
import Streaming
import Streaming.Prelude qualified as S
import System.Environment
import Web.HttpApiData (toHeader)
import Zuul.ConfigLoader (Config (..), ConnectionUrlMap, TenantResolver, emptyConfig, loadConfig)
import Zuul.ServiceConfig (ServiceConfig (..), readServiceConfig)
import Zuul.Tenant
import Zuul.ZooKeeper
import ZuulWeeder.Graph
import ZuulWeeder.Monitoring qualified
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

-- | The main function loads the config, prepare the analysis and serve the UI.
main :: IO ()
main = withUtf8 $ withLogger (\l -> getEnvArgs >>= mainWithArgs l)

mainWithArgs :: Logger -> Args -> IO ()
mainWithArgs logger args = do
  (configDump, configLoader) <- do
    configLoader <- runExceptT $ mkConfigLoader logger args.zkPath args.configPath
    case configLoader of
      Left e -> error $ Text.unpack $ "Can't load config: " <> e
      Right x -> pure x
  config <- configReloader logger configDump configLoader
  runWeb logger config

-- | Start the web interface with the "demoConfig".
-- This is useful for ghcid powered hot-reload development.
runDemo :: IO ()
runDemo = do
  withLogger $ flip runWeb demoConfig

runWeb :: Logger -> IO Analysis -> IO ()
runWeb logger config = do
  rootUrl <- ensureTrailingSlash . Text.pack . fromMaybe "/" <$> lookupEnv "WEEDER_ROOT_URL"
  distPath <- fromMaybe "dists" <$> lookupEnv "WEEDER_DIST_PATH"
  port <- maybe 9001 read <$> lookupEnv "WEEDER_PORT"
  info logger ("[+] serving 0.0.0.0:" <> toHeader port <> from rootUrl)
  let app = ZuulWeeder.UI.app config (ZuulWeeder.UI.BasePath rootUrl) distPath
  -- monitornig
  monitoring <- ZuulWeeder.Monitoring.mkMonitoring logger
  Warp.run port (monitoring app)
  where
    ensureTrailingSlash url = case Text.unsnoc url of
      Nothing -> "/"
      Just (x, '/') -> ensureTrailingSlash x
      _ -> Text.snoc url '/'

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
        Nothing -> Zuul.ZooKeeper.fetchConfigs logger dataDir serviceConfig.zookeeper
    cp = dataDir </> FilePathT "zuul/system/conf/0000000000"
    go :: ServiceConfig -> ConfigLoader
    go serviceConfig = ConfigLoader do
      -- ensure data-dir exists
      whenM (not <$> lift (doesDirectoryExist cp)) (dumpConfig $ configDumper serviceConfig)
      -- read the tenants config from dataDir
      systemConfig <- readTenantsConfig dataDir
      -- decode the tenants config
      tenantsConfig <- except (decodeTenantsConfig systemConfig)
      -- load all the config objects
      let tr = Zuul.Tenant.tenantResolver serviceConfig tenantsConfig
      config <- lift $ loadConfigFiles serviceConfig.urlBuilders tr dataDir
      pure (tenantsConfig, config)

loadConfigFiles :: ConnectionUrlMap -> TenantResolver -> FilePathT -> IO Zuul.ConfigLoader.Config
loadConfigFiles ub tr =
  flip execStateT Zuul.ConfigLoader.emptyConfig
    -- StateT Config IO ()
    . S.effects
    -- Apply the loadConfig function to each element
    . S.chain (Zuul.ConfigLoader.loadConfig ub tr)
    -- Stream (Of ZKFile) (StateT Config IO)
    . hoist lift
    -- Stream (Of ZKFile) IO
    . walkConfigNodes

-- | The demo configuration.
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
          ZKTenantsConfig
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
        tenantsConfig <- except (decodeTenantsConfig systemConfig)
        let tr = Zuul.Tenant.tenantResolver serviceConfig tenantsConfig
        conf <- lift $ flip execStateT Zuul.ConfigLoader.emptyConfig do
          xs <- sequence configFiles
          traverse_ (Zuul.ConfigLoader.loadConfig serviceConfig.urlBuilders tr) (pure <$> xs)
        pure (tenantsConfig, conf)

  let analysis = analyzeConfig tenantsConfig config
  -- pPrint analysis.config.triggers
  -- pPrint (Algebra.Graph.edgeList analysis.dependentGraph)
  pure analysis
  where
    mkConfigFile conn proj conf =
      ZKFile conn proj "main" (FilePathT ".zuul.yaml") (FilePathT "/") <$> decodeThrow conf
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

- pipeline:
    name: check
    trigger:
      timer: {}

- job:
    name: wallaby-job

- job:
    name: zena-job

- job:
    name: config-check

- project:
    check:
      jobs:
        - config-check

- project:
    name: triple-o
    check:
      jobs:
        - wallaby-job
        - zena-job
        - linter:
            nodeset: centos
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
