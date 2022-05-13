{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Zuul.Weeder (main, mainWithArgs) where

import qualified Algebra.Graph
import qualified Algebra.Graph.Export.Dot
import qualified Algebra.Graph.ToGraph
import Control.Lens ((%=))
import Control.Monad
import Control.Monad.State (State, execStateT)
import qualified Data.Map
import Data.Maybe (isNothing)
import qualified Data.Maybe
import qualified Data.Set
import Data.Text (Text, pack, unpack)
import Data.Text.Display (display)
import GHC.Generics (Generic)
import Streaming
import qualified Streaming.Prelude as S
import System.Environment
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
    ZuulConfigElement (..),
  )
import qualified Zuul.ConfigLoader
import Zuul.Tenant
  ( TenantProjects,
    ZuulConfigType (JobT, NodesetT),
    decodeTenantsConfig,
    getTenantDefaultBaseJob,
    getTenantProjects,
  )
import qualified Zuul.UI
import Zuul.ZKDump

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
    [zkPath, _configPath, "webui"] -> do
      config <- loadConfig (unpack zkPath)
      -- TODO: manage tenant, e.g.
      -- - add tenant info to config object?
      -- - enable post processing filter?
      -- - pass the proper defaultBaseJob in analyseConfig
      let graph = configRequireGraph $ analyzeConfig (\_ _ -> True) (JobName "") config
      Zuul.UI.run (toD3Graph graph)
    _ -> putStrLn "usage: zuul-weeder path"

runCommand :: FilePath -> FilePath -> TenantName -> Command -> IO ()
runCommand zkDumpPath configPath tenant command = do
  config <- loadConfig zkDumpPath
  (defaultBaseJobM, tenantProjectsM) <- loadTenantProjects zkDumpPath configPath tenant
  case (defaultBaseJobM, tenantProjectsM) of
    (Just defaultBaseJob, Just tenantProjects) -> case command of
      WhatRequire key name graph -> printReachable config defaultBaseJob tenantProjects key name graph
      WhatDependsOn key name graph -> printReachable config defaultBaseJob tenantProjects key name graph
      WhatRequireDot graph -> outputDot config defaultBaseJob tenantProjects graph
      WhatDependsOnDot graph -> outputDot config defaultBaseJob tenantProjects graph
    _ -> print $ "Unable to find tenant: " <> show tenant

outputDot :: Config -> JobName -> TenantProjects -> (Analysis -> ConfigGraph) -> IO ()
outputDot config defaultBaseJob tenantProjects graph = do
  let x = analyzeConfig (filterTenant tenantProjects) defaultBaseJob config
  let style = Algebra.Graph.Export.Dot.defaultStyle Data.Text.Display.display
  let dot = Algebra.Graph.Export.Dot.export style (graph x)
  putStrLn $ unpack dot

printReachable :: Config -> JobName -> TenantProjects -> Text -> Text -> (Analysis -> ConfigGraph) -> IO ()
printReachable config defaultBaseJob tenantProjects key name graph = do
  let vertex =
        Data.Maybe.fromMaybe (error "Can't find") $
          findVertex key name config
      analyzis = analyzeConfig (filterTenant tenantProjects) defaultBaseJob config
      reachables =
        findReachable vertex (graph analyzis)
  forM_ reachables $ \(loc, obj) -> do
    putStrLn $ unpack $ display loc <> " -> " <> display obj

loadConfig :: FilePath -> IO Zuul.ConfigLoader.Config
loadConfig =
  flip execStateT Zuul.ConfigLoader.emptyConfig
    -- StateT Config IO ()
    . S.effects
    -- Apply the loadConfig function to each element
    . S.chain Zuul.ConfigLoader.loadConfig
    -- Stream (Of ZKConfig) (StateT Config IO)
    . hoist lift
    -- Stream (Of ZKConfig) IO
    . walkConfigNodes

loadTenantProjects :: FilePath -> FilePath -> TenantName -> IO (Maybe JobName, Maybe TenantProjects)
loadTenantProjects dumpPath configPath tenantName = do
  connections <- readConnections configPath
  systemConfigE <- readSystemConfig dumpPath
  case systemConfigE of
    Left s -> error $ "Unable to read the tenant config from the ZK dump: " <> s
    Right zsc ->
      let tenantConfig = decodeTenantsConfig zsc
          defaultBaseJob = JobName <$> getTenantDefaultBaseJob tenantConfig tenantName
       in pure (defaultBaseJob, getTenantProjects connections tenantConfig tenantName)

type Vertex = (Zuul.ConfigLoader.ConfigLoc, Zuul.ConfigLoader.ZuulConfigElement)

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
      ZJob _ -> 1
      ZProjectPipeline _ -> 2
      ZNodeset _ -> 3
      ZProjectTemplate _ -> 4
      ZPipeline _ -> 5
      ZNodeLabel _ -> 6
    toLinks :: (Vertex, Vertex) -> Zuul.UI.D3Link
    toLinks ((_, a), (_, b)) = Zuul.UI.D3Link (display a) (display b)

findVertex :: Text -> Text -> Zuul.ConfigLoader.Config -> Maybe Vertex
findVertex "job" name config = case Data.Map.lookup (JobName name) (configJobs config) of
  Just [(loc, job)] -> Just (loc, ZJob job)
  _ -> Nothing
findVertex "nodeset" name config = case Data.Map.lookup (NodesetName name) (configNodesets config) of
  Just [(loc, x)] -> Just (loc, ZNodeset x)
  _ -> Nothing
findVertex "nodelabel" name config = case Data.Map.lookup (NodeLabelName name) (configNodelabels config) of
  Just [(loc, x)] -> Just (loc, ZNodeLabel x)
  _ -> Nothing
findVertex _ _ _ = Nothing

findReachable :: Vertex -> ConfigGraph -> Data.Set.Set Vertex
findReachable v = Data.Set.fromList . Algebra.Graph.ToGraph.reachable v

filterTenant :: TenantProjects -> ConfigLoc -> ZuulConfigType -> Bool
filterTenant tenantProjects (ConfigLoc (project, _, _)) itemType =
  let matches = filter (\p -> fst p == project) tenantProjects
   in case matches of
        [] -> False
        [match] -> itemType `elem` snd match
        _ -> error "Unexpected Config location not found"

analyzeConfig :: (ConfigLoc -> ZuulConfigType -> Bool) -> JobName -> Config -> Analysis
analyzeConfig filterConfig defaultBaseJob config = runIdentity $ execStateT go (Analysis Algebra.Graph.empty Algebra.Graph.empty mempty)
  where
    -- TODO: We need to handle included/excluded config type (and shadow)
    filterElems :: ZuulConfigType -> [(ConfigLoc, a)] -> [(ConfigLoc, a)]
    filterElems itemType = filter (\(cl, _) -> filterConfig cl itemType)

    setTenantDefaultParent :: [(ConfigLoc, Job)] -> [(ConfigLoc, Job)]
    setTenantDefaultParent = fmap adaptJob
      where
        adaptJob :: (ConfigLoc, Job) -> (ConfigLoc, Job)
        adaptJob (loc, job) =
          if isNothing $ jobParent job
            then (loc, job {jobParent = Just defaultBaseJob})
            else (loc, job)

    go :: State Analysis ()
    go = do
      goJobs $ setTenantDefaultParent $ filterElems JobT $ concat $ Data.Map.elems $ configJobs config
      goNodesets $ filterElems NodesetT $ concat $ Data.Map.elems $ configNodesets config

    goNodesets :: [(ConfigLoc, Nodeset)] -> State Analysis ()
    goNodesets nodesets = do
      forM_ nodesets $ \(loc, nodeset) -> do
        forM_ (nodesetLabels nodeset) $ \label -> do
          feedState ((loc, ZNodeset nodeset), (loc, ZNodeLabel label))

    goJobs :: [(ConfigLoc, Job)] -> State Analysis ()
    goJobs jobs = do
      -- TODO: filter using tenant config
      forM_ jobs $ \(loc, job) -> do
        -- look for nodeset location
        case jobNodeset job of
          Just (JobNodeset nodeset) -> case Data.Map.lookup nodeset (configNodesets config) of
            Just xs -> forM_ xs $ \(loc', ns) -> feedState ((loc, ZJob job), (loc', ZNodeset ns))
            Nothing -> #graphErrors %= (("Can't find : " <> show nodeset) :)
          _ ->
            -- Ignore inlined nodeset
            pure ()
        -- look for job parent
        case jobParent job of
          Just parent -> do
            case Data.Map.lookup parent (configJobs config) of
              Just xs -> forM_ xs $ \(loc', pj) -> feedState ((loc, ZJob job), (loc', ZJob pj))
              Nothing -> #graphErrors %= (("Can't find : " <> show parent) :)
          Nothing -> pure ()
        -- look for job dependencies
        forM_ (jobDependencies job) $ \dJob' -> do
          case Data.Map.lookup dJob' (configJobs config) of
            Just xs -> forM_ xs $ \(loc', dJob) -> feedState ((loc, ZJob job), (loc', ZJob dJob))
            Nothing -> #graphErrors %= (("Can't find : " <> show dJob') :)

    feedState :: (Vertex, Vertex) -> State Analysis ()
    feedState (a, b) = do
      #configRequireGraph %= Algebra.Graph.overlay (Algebra.Graph.edge a b)
      #configDependsOnGraph %= Algebra.Graph.overlay (Algebra.Graph.edge b a)

-- look for semaphore, secret, ...
-- pure ()
