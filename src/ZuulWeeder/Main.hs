module ZuulWeeder.Main (main, mainWithArgs, demoConfig) where

import Algebra.Graph.Export.Dot qualified
import Data.IORef
import Data.Map qualified as Map
import Data.Text (pack, unpack)
import Data.Text qualified as Text
import Options.Applicative ((<**>))
import Options.Applicative qualified as Opts
import Streaming
import Streaming.Prelude qualified as S
import System.Environment
import Text.Pretty.Simple qualified
import Zuul.ConfigLoader hiding (loadConfig)
import Zuul.ConfigLoader qualified
import Zuul.ServiceConfig (ServiceConfig (..), readServiceConfig)
import Zuul.Tenant
import Zuul.ZKDump
import ZuulWeeder.Graph
import ZuulWeeder.Prelude
import ZuulWeeder.UI qualified

data Command
  = WhatRequire Text Text TenantName
  | WhatDependsOn Text Text TenantName
  | WhatRequireDot TenantName
  | WhatDependsOnDot TenantName
  | WebUI
  | DumpConfig

data Args = Args
  { zkPath :: FilePathT,
    configPath :: FilePathT,
    command :: Command
  }

argsParser :: Opts.Parser Args
argsParser =
  Args
    <$> pathOption "data-dir" "/var/lib/zuul/weeder" "zkdump location"
    <*> pathOption "zuul-config" "/etc/zuul/zuul.conf" "zuul.conf location"
    <*> Opts.subparser
      ( commandParser "what-requires" "List what requires" whatRequireParser
          <> commandParser "what-depends" "List what depends" whatDependsParser
          <> commandParser "dot-requires" "Graph what requires" dotRequireParser
          <> commandParser "dot-depends" "Graph what depends" dotDependsParser
          <> commandParser "dump-config" "Print the decoded config" (pure DumpConfig)
          <> commandParser "serve" "Start the web service" (pure WebUI)
      )
  where
    pathOption name value help =
      FilePathT . pack <$> Opts.strOption (Opts.long name <> Opts.metavar "PATH" <> Opts.help help <> Opts.value value <> Opts.showDefault)
    strOption name =
      pack <$> Opts.strOption (Opts.long name)
    commandParser name help p = Opts.command name (Opts.info p (Opts.progDesc help))
    whatRequireParser =
      WhatRequire <$> strOption "key" <*> strOption "value" <*> (TenantName <$> strOption "tenant")
    whatDependsParser =
      WhatDependsOn <$> strOption "key" <*> strOption "value" <*> (TenantName <$> strOption "tenant")
    dotRequireParser = WhatRequireDot <$> (TenantName <$> strOption "tenant")
    dotDependsParser = WhatDependsOnDot <$> (TenantName <$> strOption "tenant")

main :: IO ()
main = getArgs >>= mainWithArgs

mainWithArgs :: [String] -> IO ()
mainWithArgs xs = do
  args <- Opts.handleParseResult $ Opts.execParserPure Opts.defaultPrefs opts xs
  mainGo args
  where
    opts =
      Opts.info
        (argsParser <**> Opts.helper)
        (Opts.fullDesc <> Opts.progDesc "Zuul weeder")

mainGo :: Args -> IO ()
mainGo args = do
  (cd, cl) <- do
    cl <- runExceptT $ configLoader args.zkPath args.configPath
    case cl of
      Left e -> error $ Text.unpack $ "Can't load config: " <> e
      Right x -> pure x
  case args.command of
    WhatRequire key name tenant -> printReachable cl tenant key name configRequireGraph
    WhatDependsOn key name tenant -> printReachable cl tenant key name configDependsOnGraph
    WhatRequireDot tenant -> outputDot cl tenant configRequireGraph
    WhatDependsOnDot tenant -> outputDot cl tenant configDependsOnGraph
    DumpConfig -> getConfig cl >>= Text.Pretty.Simple.pPrint
    WebUI -> do
      rc <- configReloader cd cl
      ZuulWeeder.UI.run do
        (tenants, config) <- rc
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
  cache <- newIORef (now, conf)
  pure (go cache)
  where
    go cache = do
      (ts, prev) <- readIORef cache
      now <- getSec
      if now - ts < 3600
        then -- conf is still fresh, we return
          pure prev
        else -- conf is stall, we reload
        do
          confE <- runExceptT do
            dumpConfig cd
            cl
          case confE of
            Left e -> do
              hPutStrLn stderr $ "Error reloading config: " <> Text.unpack e
              pure prev
            Right conf -> do
              writeIORef cache (now, conf)
              pure conf

getConfig :: ConfigLoader -> IO (TenantsConfig, Config)
getConfig cl = do
  conf <- runExceptT cl
  case conf of
    Left e -> error $ Text.unpack e
    Right x -> pure x

-- | Create IO actions to dump and load the config
configLoader :: FilePathT -> FilePathT -> ExceptT Text IO (ConfigDumper, ConfigLoader)
configLoader dataDir configFile = do
  -- Load the zuul.conf
  serviceConfig <- readServiceConfig configFile
  pure (configDumper serviceConfig, go serviceConfig)
  where
    configDumper :: ServiceConfig -> ConfigDumper
    configDumper serviceConfig = ConfigDumper do
      env <- lift $ lookupEnv "ZUUL_WEEDER_NO_ZK"
      case env of
        Just _ -> lift $ hPutStrLn stderr "[+] ZUUL_WEEDER_NO_ZK is set, skipping dumpZK"
        Nothing -> dumpZKConfig dataDir serviceConfig.zookeeper
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
      config <- lift $ loadConfig (Zuul.Tenant.tenantResolver serviceConfig tenantsConfig) dataDir
      pure (tenantsConfig, config)

outputDot :: ConfigLoader -> TenantName -> (Analysis -> ConfigGraph) -> IO ()
outputDot cl tenant graph = do
  (tenants, config) <- getConfig cl
  let x = analyzeConfig tenants config
  let style = Algebra.Graph.Export.Dot.defaultStyle display
  let g = filterTenant tenant (graph x)
  let dot = Algebra.Graph.Export.Dot.export style g
  putStrLn $ unpack dot

printReachable :: ConfigLoader -> TenantName -> Text -> Text -> (Analysis -> ConfigGraph) -> IO ()
printReachable cl tenant key name graph = do
  (tenants, config) <- getConfig cl
  let vertex =
        fromMaybe (error "Can't find") $
          findVertex key name config
      analyzis = analyzeConfig tenants config
      g = filterTenant tenant (graph analyzis)
      reachables = findReachable vertex g
  forM_ reachables $ \obj -> do
    putStrLn $ Text.unpack $ display obj

loadConfig :: TenantResolver -> FilePathT -> IO Zuul.ConfigLoader.Config
loadConfig tr =
  flip execStateT Zuul.ConfigLoader.emptyConfig
    -- StateT Config IO ()
    . S.effects
    -- Apply the loadConfig function to each element
    . S.chain (Zuul.ConfigLoader.loadConfig tr)
    -- Stream (Of ZKConfig) (StateT Config IO)
    . hoist lift
    -- Stream (Of ZKConfig) IO
    . walkConfigNodes

findVertex :: Text -> Text -> Zuul.ConfigLoader.Config -> Maybe Vertex
findVertex "job" name config = case Map.lookup (JobName name) config.jobs of
  Just [(loc, job)] -> Just (mkVertex loc job)
  _ -> Nothing
findVertex "nodeset" name config = case Map.lookup (NodesetName name) config.nodesets of
  Just [(loc, x)] -> Just (mkVertex loc x)
  _ -> Nothing
findVertex "nodelabel" name config = case Map.lookup (NodeLabelName name) config.nodeLabels of
  Just [(loc, x)] -> Just (mkVertex loc x)
  _ -> Nothing
findVertex _ _ _ = Nothing

demoConfig :: IO Analysis
demoConfig = pure $ analyzeConfig tenants config
  where
    tenants = TenantsConfig (Map.fromList [(TenantName "demo", tenantConfig)])
    tenantConfig = TenantConfig (JobName "base") mempty
    config = emptyConfig & #jobs `set` Map.fromList [mkJob "base", mkJob "linters"]
    mkJob (JobName -> n) = (n, [(demoLoc, Job n Nothing Nothing [] [])])
    demoLoc = ConfigLoc (CanonicalProjectName demoProject) (BranchName "main") ".zuul.yaml" [TenantName "demo"]
    demoProject = (ProviderName "sftests.com", ProjectName "config")
