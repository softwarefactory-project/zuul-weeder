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

import Control.Concurrent.CGroup qualified
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Yaml (decodeThrow)
import Network.Wai.Handler.Warp qualified as Warp
import Streaming
import Streaming.Prelude qualified as S
import System.Environment
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM)
import Web.HttpApiData (toHeader)
import Zuul.Config (CanonicalProjectName, TenantName)
import Zuul.ConfigLoader (Config (..), ConnectionUrlMap, emptyConfig, loadConfig, postProcess)
import Zuul.ServiceConfig (ServiceConfig (..), readServiceConfig)
import Zuul.Tenant
import Zuul.ZooKeeper
import ZuulWeeder.Graph
import ZuulWeeder.Monitoring qualified
import ZuulWeeder.Prelude
import ZuulWeeder.UI qualified
import ZuulWeeder.UI.App (CacheRender)
import ZuulWeeder.UI.App qualified

data Args = Args
  { zkPath :: FilePathT
  , configPath :: FilePathT
  }

getEnvArgs :: IO Args
getEnvArgs =
  Args <$> envPath "WEEDER_DATA" "/var/tmp/weeder" <*> envPath "ZUUL_CONF" "/etc/zuul/zuul.conf"
 where
  envPath :: String -> FilePath -> IO FilePathT
  envPath name def = FilePathT . Text.pack . fromMaybe def <$> lookupEnv name

-- | The main function loads the config, prepare the analysis and serve the UI.
main :: IO ()
main = do
  Control.Concurrent.CGroup.initRTSThreads
  withUtf8 $ withLogger (\l -> getEnvArgs >>= mainWithArgs l)

mainWithArgs :: Logger -> Args -> IO ()
mainWithArgs logger args = do
  (configDump, configLoader) <- do
    configLoader <- runExceptT $ mkConfigLoader logger args.zkPath args.configPath
    case configLoader of
      Left e -> error $ Text.unpack $ "Can't load config: " <> e
      Right x -> pure x
  cacheRender <- newMVar mempty
  config <- configReloader logger configDump configLoader cacheRender
  runWeb logger config cacheRender

-- | Start the web interface with the "demoConfig".
-- This is useful for ghcid powered hot-reload development.
runDemo :: IO ()
runDemo = do
  cacheRender <- newMVar mempty
  withLogger $ \logger -> runWeb logger demoConfig cacheRender

runWeb :: Logger -> IO AnalysisStatus -> MVar CacheRender -> IO ()
runWeb logger config cacheRender = do
  rootUrl <- ensureTrailingSlash . Text.pack . fromMaybe "/" <$> lookupEnv "WEEDER_ROOT_URL"
  distPath <- fromMaybe "dists" <$> lookupEnv "WEEDER_DIST_PATH"
  port <- maybe 9001 read <$> lookupEnv "WEEDER_PORT"
  info logger ("[+] serving 0.0.0.0:" <> toHeader port <> encodeUtf8 rootUrl)
  let app = ZuulWeeder.UI.App.app config cacheRender (ZuulWeeder.UI.BasePath rootUrl) distPath
  -- monitornig
  monitoring <- ZuulWeeder.Monitoring.mkMonitoring logger
  Warp.runSettings (wsettings port) (monitoring app)
 where
  wsettings port = Warp.defaultSettings & Warp.setPort port & Warp.setInstallShutdownHandler tHandler
  tHandler closeSocket = void $ installHandler sigTERM (Catch closeSocket) Nothing
  ensureTrailingSlash url = case Text.unsnoc url of
    Nothing -> "/"
    Just (x, '/') -> ensureTrailingSlash x
    _ -> Text.snoc url '/'

newtype ConfigDumper = ConfigDumper {dumpConfig :: ExceptT Text IO ()}

newtype ConfigLoader = ConfigLoader {loadConfig :: ExceptT Text IO (TenantsConfig, Config)}

-- | Create a IO action that reloads the config every hour.
configReloader :: Logger -> ConfigDumper -> ConfigLoader -> MVar CacheRender -> IO (IO AnalysisStatus)
configReloader logger configDumper configLoader cacheRender = do
  -- Get current time
  now <- getSec
  -- Read the inital conf, error is fatal here
  conf <- either (error . Text.unpack) id <$> runExceptT (configLoader.loadConfig)
  -- Cache the result
  cache <- newIORef (newAnalysisStatus $ uncurry analyzeConfig conf)
  ts <- newMVar (now, cache)
  pure (modifyMVar ts go)
 where
  go :: (Int64, IORef AnalysisStatus) -> IO ((Int64, IORef AnalysisStatus), AnalysisStatus)
  go (ts, cache) = do
    status <- readIORef cache
    now <- getSec
    if now - ts < 3600
      then pure ((ts, cache), status)
      else do
        modifyIORef cache (#refreshing `set` True)
        reload cache
        pure ((now, cache), status)

  -- Load the config in a background thread
  reload :: IORef AnalysisStatus -> IO ()
  reload cache = void $ forkIO do
    let setError err = modifyIORef cache ((#loadingError `set` Just err) . (#refreshing `set` False))
    res <- timeout 600_000_000 $ do
      info logger "ReLoading the configuration"
      confE <- runExceptT do
        configDumper.dumpConfig
        configLoader.loadConfig
      case confE of
        Left e -> do
          info logger ("Error reloading config: " <> encodeUtf8 e)
          setError e
        Right conf -> do
          info logger "Caching the graph result"
          writeIORef cache (newAnalysisStatus $ uncurry analyzeConfig conf)
          modifyMVar cacheRender (\_ -> pure (mempty, ()))
    when (isNothing res) do
      info logger "Error reloading config timeout"
      setError "Loading config timeout"

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
    let tr = Zuul.Tenant.mkResolver serviceConfig tenantsConfig
        allTenants = Set.fromList $ Map.keys tenantsConfig.tenants
        allProjects = getCanonicalProjects serviceConfig.connections tenantsConfig
    config <- lift $ loadConfigFiles allProjects allTenants serviceConfig.urlBuilders tr dataDir
    pure (tenantsConfig, Zuul.ConfigLoader.postProcess config)

loadConfigFiles :: Map CanonicalProjectName (Set TenantName) -> Set TenantName -> ConnectionUrlMap -> TenantResolver -> FilePathT -> IO Zuul.ConfigLoader.Config
loadConfigFiles projs tenants ub tr =
  flip execStateT (Zuul.ConfigLoader.emptyConfig ub projs tenants)
    -- StateT Config IO ()
    . S.effects
    -- Apply the loadConfig function to each element
    . S.chain (Zuul.ConfigLoader.loadConfig ub tr)
    -- Stream (Of ZKFile) (StateT Config IO)
    . hoist lift
    -- Stream (Of ZKFile) IO
    . walkConfigNodes

-- | The demo configuration.
demoConfig :: IO AnalysisStatus
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

[connection "gerrit"]
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
            - triple-o
            - zuul-jobs:
                include: [job]
                shadow: sf-jobs
|]
        tenantsConfig <- except (decodeTenantsConfig systemConfig)
        let tr = Zuul.Tenant.mkResolver serviceConfig tenantsConfig
            allTenants = Set.fromList $ Map.keys tenantsConfig.tenants
            allProjects = getCanonicalProjects serviceConfig.connections tenantsConfig
            initialConfig = Zuul.ConfigLoader.emptyConfig serviceConfig.urlBuilders allProjects allTenants
        conf <- lift $ flip execStateT initialConfig do
          xs <- sequence configFiles
          traverse_ (Zuul.ConfigLoader.loadConfig serviceConfig.urlBuilders tr) (pure <$> xs)
        pure (tenantsConfig, Zuul.ConfigLoader.postProcess conf)

  let analysis = analyzeConfig tenantsConfig config
  -- pPrint analysis.config.triggers
  -- pPrint (Algebra.Graph.edgeList analysis.dependentGraph)
  pure (newAnalysisStatus analysis)
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
    abstract: true
    nodeset: centos
    secrets: log-key

- secret:
    name: log-key

- nodeset:
    name: centos
    nodes:
      - name: runner
        label: cloud-centos-7

- queue:
    name: queue

- pipeline:
    name: check
    trigger:
      gerrit: {}
    success:
      elastic:

- pipeline:
    name: periodic
    trigger:
      timer:
        - time: '0 8 * * 6'

- job:
    name: wallaby-job
    required-projects:
      - triple-o

- job:
    name: zena-job

- job:
    name: config-check

- project-template:
    name: common
    check:
      jobs:
        - linter

- project:
    templates:
      - common
    check:
      jobs:
        - config-check

- project:
    name: triple-o
    queue: queue
    check:
      jobs:
        - wallaby-job
        - zena-job
        - linter:
            nodeset: centos
|]
    , mkConfigFile
        "sftests.com"
        "sf-jobs"
        [s|
- job:
    name: linter
    nodeset:
      nodes:
        - name: container
          label: pod-centos-7
|]
    , mkConfigFile
        "localhost"
        "project-config"
        [s|
- job:
    name: base
    nodeset: rhel
    semaphores: testy-sem

- pipeline:
    name: check
    trigger:
      gerrit: {}
    success:
      elastic:

- nodeset:
    name: rhel
    nodes:
      - name: runner
        label: cloud-rhel-7
|]
    ]
