module ZuulWeeder.Main (main, mainWithArgs) where

import Algebra.Graph qualified
import Algebra.Graph.Export.Dot qualified
import Data.Map qualified as Map
import Data.Text (pack, unpack)
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
import ZuulWeeder.UI qualified
import Zuul.ZKDump
import ZuulWeeder.Prelude
import ZuulWeeder.Graph

data Command
  = WhatRequire Text Text (Analysis -> ConfigGraph)
  | WhatDependsOn Text Text (Analysis -> ConfigGraph)
  | WhatRequireDot (Analysis -> ConfigGraph)
  | WhatDependsOnDot (Analysis -> ConfigGraph)

-- TODO: Implement a proper command line parser
main :: IO ()
main = do
  args <- map pack <$> getArgs
  mainWithArgs args

mainWithArgs :: [Text] -> IO ()
mainWithArgs args =
  case args of
    [zkPath, configPath, tenant, "what-require", key, name] -> do
      let cmd = WhatRequire key name configRequireGraph
       in runCommand (unpack zkPath) (unpack configPath) (TenantName tenant) cmd
    [zkPath, configPath, tenant, "what-depends-on", key, name] -> do
      let cmd = WhatDependsOn key name configDependsOnGraph
       in runCommand (unpack zkPath) (unpack configPath) (TenantName tenant) cmd
    [zkPath, configPath, tenant, "what-require-dot"] -> do
      let cmd = WhatRequireDot configRequireGraph
       in runCommand (unpack zkPath) (unpack configPath) (TenantName tenant) cmd
    [zkPath, configPath, tenant, "what-depends-on-dot"] -> do
      let cmd = WhatDependsOnDot configDependsOnGraph
       in runCommand (unpack zkPath) (unpack configPath) (TenantName tenant) cmd
    [unpack -> zkPath, unpack -> configPath, "webui"] -> do
      (tenants, tr) <- loadSystemConfig zkPath configPath
      config <- loadConfig tr zkPath
      let analysis = analyzeConfig tenants config
      let graph = configRequireGraph analysis
      ZuulWeeder.UI.run (toD3Graph graph)
    [unpack -> zkPath, unpack -> configPath, "dump"] -> do
      (_, tr) <- loadSystemConfig zkPath configPath
      config <- loadConfig tr zkPath
      Text.Pretty.Simple.pPrint config
    _ -> putStrLn "usage: zuul-weeder path"

runCommand :: FilePath -> FilePath -> TenantName -> Command -> IO ()
runCommand zkDumpPath configPath tenant command = do
  (tenants, tr) <- loadSystemConfig zkDumpPath configPath
  config <- loadConfig tr zkDumpPath

  case command of
    WhatRequire key name graph -> printReachable config tenant tenants key name graph
    WhatDependsOn key name graph -> printReachable config tenant tenants key name graph
    WhatRequireDot graph -> outputDot config tenant tenants graph
    WhatDependsOnDot graph -> outputDot config tenant tenants graph

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

loadConfig :: TenantResolver -> FilePath -> IO Zuul.ConfigLoader.Config
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

loadSystemConfig :: FilePath -> FilePath -> IO (TenantsConfig, TenantResolver)
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
