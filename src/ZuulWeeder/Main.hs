module ZuulWeeder.Main (main, mainWithArgs, demoConfig) where

import Control.Concurrent.MVar
import Data.Text qualified as Text
import Data.Yaml (decodeThrow)
import Streaming
import Streaming.Prelude qualified as S
import System.Environment
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
main = getEnvArgs >>= mainWithArgs

mainWithArgs :: Args -> IO ()
mainWithArgs args = do
  (cd, cl) <- do
    cl <- runExceptT $ configLoader args.zkPath args.configPath
    case cl of
      Left e -> error $ Text.unpack $ "Can't load config: " <> e
      Right x -> pure x
  rc <- configReloader cd cl
  ZuulWeeder.UI.run do
    info "Loading the configuration"
    (tenants, config) <- rc
    info "Building the graph"
    pure $ analyzeConfig tenants config

newtype ConfigDumper = ConfigDumper {dumpConfig :: ExceptT Text IO ()}

type ConfigLoader = ExceptT Text IO (TenantsConfig, Config)

-- | Create a IO action that reloads the config every hour.
configReloader :: ConfigDumper -> ConfigLoader -> IO (IO (TenantsConfig, Config))
configReloader cd cl = do
  -- Get current time
  now <- getSec
  -- Read the inital conf, error is fatal here
  conf <- either (error . Text.unpack) id <$> runExceptT cl
  -- Cache the result
  cache <- newMVar (now, conf)
  pure (modifyMVar cache go)
  where
    go cache@(ts, prev) = do
      now <- getSec
      if now - ts < 3600
        then -- conf is still fresh, we return
          pure (cache, prev)
        else -- conf is stall, we reload
        do
          confE <- runExceptT do
            dumpConfig cd
            cl
          case confE of
            Left e -> do
              hPutStrLn stderr $ "Error reloading config: " <> Text.unpack e
              pure ((now, prev), prev)
            Right conf -> do
              pure ((now, conf), conf)

-- | Create IO actions to dump and load the config
configLoader :: FilePathT -> FilePathT -> ExceptT Text IO (ConfigDumper, ConfigLoader)
configLoader dataDir configFile = do
  -- Load the zuul.conf
  serviceConfig <- readServiceConfig (readFileText configFile)
  pure (configDumper serviceConfig, go serviceConfig)
  where
    configDumper :: ServiceConfig -> ConfigDumper
    configDumper serviceConfig = ConfigDumper do
      env <- lift $ lookupEnv "ZUUL_WEEDER_NO_ZK"
      case env of
        Just _ -> lift $ hPutStrLn stderr "[+] ZUUL_WEEDER_NO_ZK is set, skipping dumpZK"
        Nothing -> dumpZKConfig (dataDir </> "data") serviceConfig.zookeeper
    cp = dataDir </> FilePathT "zuul/system/conf/0000000000"
    go :: ServiceConfig -> ExceptT Text IO (TenantsConfig, Config)
    go serviceConfig = do
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
