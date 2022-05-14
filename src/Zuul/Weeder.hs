module Zuul.Weeder (main, mainWithArgs) where

import Algebra.Graph qualified
import Algebra.Graph.Export.Dot qualified
import Algebra.Graph.ToGraph qualified
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (pack, unpack)
import Streaming
import Streaming.Prelude qualified as S
import System.Environment
import Text.Pretty.Simple qualified
import Zuul.Config (readConnections)
import Zuul.ConfigLoader
  ( Config (..),
    ConfigLoc (..),
    Job (..),
    JobName (..),
    JobNodeset (..),
    NodeLabelName (NodeLabelName),
    Nodeset (..),
    NodesetName (..),
    TenantName (TenantName),
    TenantResolver,
    ZuulConfigElement (..),
  )
import Zuul.ConfigLoader qualified
import Zuul.Tenant
  ( TenantsConfig,
    decodeTenantsConfig,
    tenantResolver,
  )
import Zuul.Tenant qualified
import Zuul.UI qualified
import Zuul.ZKDump
import ZuulWeeder.Prelude

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
      Zuul.UI.run (toD3Graph graph)
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

filterTenant :: TenantName -> ConfigGraph -> ConfigGraph
filterTenant tenant = Algebra.Graph.induce (\(loc, _) -> tenant `elem` loc.tenants)

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

-- | The graph vertex
-- Node label names are added to the raw zuul config element so that they can be searched independently.
data ConfigVertex
  = ZuulConfigVertex ZuulConfigElement
  | NodeLabelVertex NodeLabelName
  deriving (Eq, Ord, Show, Generic)

instance Display ConfigVertex where
  displayBuilder (NodeLabelVertex p) = displayBuilder p
  displayBuilder (ZuulConfigVertex p) = displayBuilder p

pattern VNodeLabel :: NodeLabelName -> ConfigVertex
pattern VNodeLabel x = NodeLabelVertex x

pattern VJob :: Job -> ConfigVertex
pattern VJob x = ZuulConfigVertex (ZJob x)

pattern VProjectPipeline :: Zuul.ConfigLoader.ProjectPipeline -> ConfigVertex
pattern VProjectPipeline x = ZuulConfigVertex (ZProjectPipeline x)

pattern VNodeset :: Nodeset -> ConfigVertex
pattern VNodeset x = ZuulConfigVertex (ZNodeset x)

pattern VProjectTemplate :: Zuul.ConfigLoader.ProjectPipeline -> ConfigVertex
pattern VProjectTemplate x = ZuulConfigVertex (ZProjectTemplate x)

pattern VPipeline :: Zuul.ConfigLoader.Pipeline -> ConfigVertex
pattern VPipeline x = ZuulConfigVertex (ZPipeline x)
{-# COMPLETE VNodeLabel, VJob, VProjectPipeline, VNodeset, VProjectTemplate, VPipeline #-}

type Vertex = (ConfigLoc, ConfigVertex)

type ConfigGraph = Algebra.Graph.Graph Vertex

data Analysis = Analysis
  { configRequireGraph :: ConfigGraph,
    configDependsOnGraph :: ConfigGraph,
    graphErrors :: [String]
  }
  deriving (Show, Generic)

toD3Graph :: ConfigGraph -> Zuul.UI.D3Graph
toD3Graph g =
  Zuul.UI.D3Graph
    { Zuul.UI.nodes = toNodes <$> Algebra.Graph.vertexList g,
      Zuul.UI.links = toLinks <$> Algebra.Graph.edgeList g
    }
  where
    toNodes :: Vertex -> Zuul.UI.D3Node
    toNodes (_, e) = Zuul.UI.D3Node (display e) $ case e of
      VJob _ -> 1
      VProjectPipeline _ -> 2
      VNodeset _ -> 3
      VProjectTemplate _ -> 4
      VPipeline _ -> 5
      VNodeLabel _ -> 6
    toLinks :: (Vertex, Vertex) -> Zuul.UI.D3Link
    toLinks ((_, a), (_, b)) = Zuul.UI.D3Link (display a) (display b)

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

findReachable :: Vertex -> ConfigGraph -> Set.Set Vertex
findReachable v = Set.fromList . Algebra.Graph.ToGraph.reachable v

analyzeConfig :: TenantsConfig -> Config -> Analysis
analyzeConfig (Zuul.Tenant.TenantsConfig tenantsConfig) config =
  runIdentity $ execStateT go (Analysis Algebra.Graph.empty Algebra.Graph.empty mempty)
  where
    -- All the default base jobs defined by the tenants
    -- Given:
    -- - tenant1, tenant2 default base job is 'base'
    -- - tenant3 default base job is 'base-minimal'
    -- Then: baseJobs = [(base, [tenant1, tenant2]), (base-minimal, [tenant3])]
    baseJobs :: [(JobName, [TenantName])]
    baseJobs = Map.toList baseJobsMap
      where
        baseJobsMap :: Map.Map JobName [TenantName]
        baseJobsMap = foldr insertTenant mempty (Map.toList tenantsConfig)
        insertTenant (tenantName, tenantConfig) =
          Map.insertWith mappend tenantConfig.defaultParent [tenantName]

    -- The job list, where the tenant parent job is applied.
    -- Given:
    -- - job1: parent is Nothing
    -- - job2: parent is Just job1
    -- Then: allJobs = fromList
    --   [ (job1, [ (loc {tenants = [tenant1, tenant2]}, job1 {parent = Just base})
    --            , (loc {tenants = [tenant3]}, job1 {parent = Just base-minimal}) ])
    --   , (job2, [ (loc, [job2]) ]) ]
    allJobs :: Zuul.ConfigLoader.ConfigMap JobName Job
    allJobs = Map.map (concatMap expandBaseJobs) config.jobs
      where
        expandBaseJobs :: (ConfigLoc, Job) -> [(ConfigLoc, Job)]
        expandBaseJobs (loc, job)
          -- When parent is set, we don't touch the job
          | isJust job.parent = [(loc, job)]
          -- Otherwise we set the parent for each tenant
          | otherwise = mapMaybe (setParentJob (loc, job)) baseJobs
        setParentJob :: (ConfigLoc, Job) -> (JobName, [TenantName]) -> Maybe (ConfigLoc, Job)
        setParentJob (loc, job) (parent, tenants)
          -- The default base job is from other tenants
          | all (`notElem` loc.tenants) tenants = Nothing
          -- This job is the base job, we don't set it's parent
          | job.name == parent = Just (loc, job)
          | otherwise = Just (loc {tenants = tenants}, job {parent = Just parent})

    go :: State Analysis ()
    go = do
      goJobs $ concat $ Map.elems allJobs
      goNodesets $ concat $ Map.elems config.nodesets

    -- TODO: implement a lookup function that check matching tenant. Otherwise we might incorrectly link objects between tenants

    goNodesets :: [(ConfigLoc, Nodeset)] -> State Analysis ()
    goNodesets nodesets = do
      forM_ nodesets $ \(loc, nodeset) -> do
        forM_ nodeset.labels $ \label -> do
          feedState ((loc, VNodeset nodeset), (loc, VNodeLabel label))

    goJobs :: [(ConfigLoc, Job)] -> State Analysis ()
    goJobs jobs = do
      -- TODO: filter using tenant config
      forM_ jobs $ \(loc, job) -> do
        -- look for nodeset location
        case job.nodeset of
          Just (JobNodeset nodeset) -> case Map.lookup nodeset config.nodesets of
            Just xs -> forM_ xs $ \(loc', ns) -> feedState ((loc, VJob job), (loc', VNodeset ns))
            Nothing -> #graphErrors %= (("Can't find : " <> show nodeset) :)
          _ ->
            -- Ignore inlined nodeset
            pure ()
        -- look for job parent
        case job.parent of
          Just parent -> do
            case Map.lookup parent allJobs of
              Just xs -> forM_ xs $ \(loc', pj) -> feedState ((loc, VJob job), (loc', VJob pj))
              Nothing -> #graphErrors %= (("Can't find : " <> show parent) :)
          Nothing -> pure ()
        -- look for job dependencies
        forM_ job.dependencies $ \dJob' -> do
          case Map.lookup dJob' allJobs of
            Just xs -> forM_ xs $ \(loc', dJob) -> feedState ((loc, VJob job), (loc', VJob dJob))
            Nothing -> #graphErrors %= (("Can't find : " <> show dJob') :)

    feedState :: (Vertex, Vertex) -> State Analysis ()
    feedState (a, b) = do
      #configRequireGraph %= Algebra.Graph.overlay (Algebra.Graph.edge a b)
      #configDependsOnGraph %= Algebra.Graph.overlay (Algebra.Graph.edge b a)

-- look for semaphore, secret, ...
-- pure ()
