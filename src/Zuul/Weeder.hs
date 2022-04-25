{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Zuul.Weeder (main) where

import qualified Algebra.Graph
import qualified Algebra.Graph.Export.Dot
import qualified Algebra.Graph.ToGraph
import Control.Lens ((%=))
import Control.Monad
import Control.Monad.State (State, execStateT)
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Set
import Data.Text (Text, pack, unpack)
import Data.Text.Display (display)
import Debug.Trace
import GHC.Generics (Generic)
import Streaming
import qualified Streaming.Prelude as S
import System.Environment
import Zuul.ConfigLoader
  ( Config (..),
    ConfigLoc (..),
    Job (..),
    JobName (..),
    JobNodeset (..),
    NodeLabelName (NodeLabelName),
    Nodeset (..),
    NodesetName (..),
    ZuulConfigElement (..),
  )
import qualified Zuul.ConfigLoader
import Zuul.ZKDump

main :: IO ()
main = do
  args <- map pack <$> getArgs
  case args of
    [path, "what-require", key, name] -> printReachable (unpack path) key name configRequireGraph
    [path, "what-depends-on", key, name] -> printReachable (unpack path) key name configDependsOnGraph
    [path, "require-dot"] -> outputDot (unpack path) configRequireGraph
    [path, "depends-on-dot"] -> outputDot (unpack path) configDependsOnGraph
    _ -> putStrLn "usage: zuul-weeder path"

-- TODO
tenant :: Tenant
tenant = Tenant

outputDot :: FilePath -> (Analysis -> ConfigGraph) -> IO ()
outputDot path graph = do
  config <- loadConfig path
  let x = analyzeConfig (filterTenant tenant) config
  let style = Algebra.Graph.Export.Dot.defaultStyle Data.Text.Display.display
  let dot = Algebra.Graph.Export.Dot.export style (graph x)
  putStrLn $ unpack dot

printReachable :: FilePath -> Text -> Text -> (Analysis -> ConfigGraph) -> IO ()
printReachable path key name graph = do
  config <- loadConfig path
  let vertex =
        Data.Maybe.fromMaybe (error "Can't find") $
          findVertex key name config
      analyzis = analyzeConfig (filterTenant tenant) config
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

type Vertex = (Zuul.ConfigLoader.ConfigLoc, Zuul.ConfigLoader.ZuulConfigElement)

type ConfigGraph = Algebra.Graph.Graph Vertex

data Analysis = Analysis
  { configRequireGraph :: ConfigGraph,
    configDependsOnGraph :: ConfigGraph,
    graphErrors :: [String]
  }
  deriving (Show, Generic)

findVertex :: Text -> Text -> Zuul.ConfigLoader.Config -> Maybe Vertex
findVertex "job" name config = case Data.Map.lookup (JobName name) (configJobs config) of
  Just [(loc, job)] -> Just (loc, ZJob job)
  _ -> Nothing
findVertex "nodeset" name config = case Data.Map.lookup (NodesetName name) (configNodesets config) of
  Just [(loc, x)] -> Just (loc, ZNodeset x)
  _ -> Nothing
findVertex "nodelabel" name config = case Data.Map.lookup (NodeLabelName name) (configNodelabels config) of
  Just [(loc, x)] -> Just (loc, ZNodeLabel x)
  _ -> trace (show $ configNodelabels config) Nothing
findVertex _ _ _ = Nothing

findReachable :: Vertex -> ConfigGraph -> Data.Set.Set Vertex
findReachable v = Data.Set.fromList . Algebra.Graph.ToGraph.reachable v

data Tenant = Tenant -- TODO, move to a TenantConfig module

filterTenant :: Tenant -> ConfigLoc -> Bool
filterTenant _ = const True

analyzeConfig :: (ConfigLoc -> Bool) -> Config -> Analysis
analyzeConfig filterConfig config = runIdentity $ execStateT go (Analysis Algebra.Graph.empty Algebra.Graph.empty mempty)
  where
    filterElems :: [(ConfigLoc, a)] -> [(ConfigLoc, a)]
    filterElems = filter (\(cl, _) -> filterConfig cl)

    go :: State Analysis ()
    go = do
      goJobs $ filterElems $ concat $ Data.Map.elems $ configJobs config
      goNodesets $ filterElems $ concat $ Data.Map.elems $ configNodesets config

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
