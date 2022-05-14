module ZuulWeeder.Main (main, mainWithArgs) where

import Algebra.Graph qualified
import Algebra.Graph.Export.Dot qualified
import Data.Map qualified as Map
import Data.Text (pack, unpack)
import Options.Applicative ((<**>))
import Options.Applicative qualified as Opts
import Streaming
import Streaming.Prelude qualified as S
import System.Environment
import Text.Pretty.Simple qualified
import Zuul.Config (readConnections)
import Zuul.ConfigLoader
  ( Config (..),
    JobName (..),
    NodeLabelName (NodeLabelName),
    NodesetName (..),
    TenantName (TenantName),
    TenantResolver,
  )
import Zuul.ConfigLoader qualified
import Zuul.Tenant
  ( TenantsConfig,
    decodeTenantsConfig,
    tenantResolver,
  )
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
    <$> pathOption "zk-dump" "zkdump location"
    <*> pathOption "zuul-config" "zuul.conf location"
    <*> Opts.subparser
      ( commandParser "what-requires" "List what requires" whatRequireParser
          <> commandParser "what-depends" "List what depends" whatDependsParser
          <> commandParser "dot-requires" "Graph what requires" dotRequireParser
          <> commandParser "dot-depends" "Graph what depends" dotDependsParser
          <> commandParser "dump-config" "Print the decoded config" (pure DumpConfig)
          <> commandParser "serve" "Start the web service" (pure WebUI)
      )
  where
    pathOption name help =
      FilePathT . pack <$> Opts.strOption (Opts.long name <> Opts.metavar "PATH" <> Opts.help help)
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
  (tenants, tr) <- loadSystemConfig args.zkPath args.configPath
  config <- loadConfig tr args.zkPath

  case args.command of
    WhatRequire key name tenant -> printReachable config tenant tenants key name configRequireGraph
    WhatDependsOn key name tenant -> printReachable config tenant tenants key name configDependsOnGraph
    WhatRequireDot tenant -> outputDot config tenant tenants configRequireGraph
    WhatDependsOnDot tenant -> outputDot config tenant tenants configDependsOnGraph
    DumpConfig -> Text.Pretty.Simple.pPrint config
    WebUI -> do
      let analysis = analyzeConfig tenants config
      let graph = configRequireGraph analysis
      ZuulWeeder.UI.run (toD3Graph graph)

outputDot :: Config -> TenantName -> TenantsConfig -> (Analysis -> ConfigGraph) -> IO ()
outputDot config tenant tenants graph = do
  let x = analyzeConfig tenants config
  let style = Algebra.Graph.Export.Dot.defaultStyle display
  let g = filterTenant tenant (graph x)
  let dot = Algebra.Graph.Export.Dot.export style g
  putStrLn $ unpack dot

printReachable :: Config -> TenantName -> TenantsConfig -> Text -> Text -> (Analysis -> ConfigGraph) -> IO ()
printReachable config tenant tenants key name graph = do
  let vertex =
        fromMaybe (error "Can't find") $
          findVertex key name config
      analyzis = analyzeConfig tenants config
      g = filterTenant tenant (graph analyzis)
      reachables = findReachable vertex g
  forM_ reachables $ \(loc, obj) -> do
    putStrLn $ unpack $ display loc <> " -> " <> display obj

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

loadSystemConfig :: FilePathT -> FilePathT -> IO (TenantsConfig, TenantResolver)
loadSystemConfig dumpPath configPath = do
  connections <- readConnections configPath
  systemConfigE <- readSystemConfig dumpPath
  pure $ case systemConfigE of
    Left s -> error $ "Unable to read the tenant config from the ZK dump: " <> s
    Right (decodeTenantsConfig -> Just zsc) ->
      (zsc, Zuul.Tenant.tenantResolver zsc connections)
    _ -> error "Invalid tenant config"

toD3Graph :: ConfigGraph -> ZuulWeeder.UI.D3Graph
toD3Graph g =
  ZuulWeeder.UI.D3Graph
    { ZuulWeeder.UI.nodes = toNodes <$> Algebra.Graph.vertexList g,
      ZuulWeeder.UI.links = toLinks <$> Algebra.Graph.edgeList g
    }
  where
    toNodes :: Vertex -> ZuulWeeder.UI.D3Node
    toNodes (_, e) = ZuulWeeder.UI.D3Node (display e) $ case e of
      VJob _ -> 1
      VProjectPipeline _ -> 2
      VNodeset _ -> 3
      VProjectTemplate _ -> 4
      VPipeline _ -> 5
      VNodeLabel _ -> 6
      VQueue _ -> 7
      VSemaphore _ -> 8
    toLinks :: (Vertex, Vertex) -> ZuulWeeder.UI.D3Link
    toLinks ((_, a), (_, b)) = ZuulWeeder.UI.D3Link (display a) (display b)

findVertex :: Text -> Text -> Zuul.ConfigLoader.Config -> Maybe Vertex
findVertex "job" name config = case Map.lookup (JobName name) config.jobs of
  Just [(loc, job)] -> Just (loc, VJob job)
  _ -> Nothing
findVertex "nodeset" name config = case Map.lookup (NodesetName name) config.nodesets of
  Just [(loc, x)] -> Just (loc, VNodeset x)
  _ -> Nothing
findVertex "nodelabel" name config = case Map.lookup (NodeLabelName name) config.nodeLabels of
  Just [(loc, x)] -> Just (loc, VNodeLabel x)
  _ -> Nothing
findVertex _ _ _ = Nothing
