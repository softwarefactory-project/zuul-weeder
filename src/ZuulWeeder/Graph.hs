-- |
-- Module      : ZuulWeeder.Graph
-- Description : Configuration graph
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
--
-- Maintainer  : tdecacqu@redhat.com, fboucher@redhat.com
-- Stability   : provisional
-- Portability : portable
--
-- This module contains the core configuration graph.
module ZuulWeeder.Graph
  ( ConfigGraph,
    Analysis (..),
    Vertex (..),
    VertexName (..),
    vertexTypeName,
    analyzeConfig,
    findReachable,
  )
where

import Algebra.Graph qualified
import Algebra.Graph.ToGraph qualified
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Zuul.Config
import Zuul.ConfigLoader (Config (..), ConfigMap)
import Zuul.Tenant
import ZuulWeeder.Prelude

-- | The graph vertex
data Vertex = Vertex
  { -- | The vertex identifier
    name :: VertexName,
    -- | The list of tenants using that vertex
    tenants :: Set TenantName
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | A Vertex can be a raw zuul config element, or a custom element added through analysis
data VertexName
  = -- | A job
    VJob JobName
  | -- | A nodeset
    VNodeset NodesetName
  | -- | A node label
    VNodeLabel NodeLabelName
  | -- | A project
    VProject ProjectName
  | -- | A project template
    VProjectTemplate ProjectTemplateName
  | -- | A pipeline
    VPipeline PipelineName
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | A text representation of a vertex type, useful for /object url piece.
vertexTypeName :: VertexName -> Text
vertexTypeName = \case
  VJob _ -> "job"
  VNodeset _ -> "nodeset"
  VNodeLabel _ -> "label"
  VProject _ -> "project"
  VProjectTemplate _ -> "project-template"
  VPipeline _ -> "pipeline"

instance From VertexName Text where
  from = \case
    VJob (JobName n) -> n
    VNodeset (NodesetName n) -> n
    VNodeLabel (NodeLabelName n) -> n
    VProject (ProjectName n) -> n
    VProjectTemplate (ProjectTemplateName n) -> n
    VPipeline (PipelineName n) -> n

instance From Job VertexName where
  from job = VJob job.name

instance From Project VertexName where
  from pp = VProject pp.name

instance From ProjectTemplate VertexName where
  from p = VProjectTemplate p.name

instance From Pipeline VertexName where
  from p = VPipeline p.name

instance From Nodeset VertexName where
  from p = VNodeset p.name

instance From NodeLabelName VertexName where
  from = VNodeLabel

mkVertex :: From a VertexName => ConfigLoc -> a -> Vertex
mkVertex loc x = Vertex (from x) loc.tenants

-- | A convenient type alias.
type ConfigGraph = Algebra.Graph.Graph Vertex

-- | Return the list of reachable 'Vertex'
findReachable ::
  -- | The list of 'Vertex' to search
  NonEmpty Vertex ->
  -- | The graph to search in
  ConfigGraph ->
  -- | The list of reachable 'Vertex'
  Set Vertex
findReachable xs = Set.fromList . filter (/= v) . Algebra.Graph.ToGraph.dfs (NE.toList xs)
  where
    v = NE.head xs

-- | The config analysis result used by the "ZuulWeeder.UI" module.
data Analysis = Analysis
  { -- | The requirements graph
    configRequireGraph :: ConfigGraph,
    -- | The depends-on graph
    configDependsOnGraph :: ConfigGraph,
    -- | The list of vertex, used for displaying search result.
    vertices :: Set Vertex,
    -- | A map of all the names and their matching tenants, used for searching.
    names :: Map VertexName (Set TenantName),
    -- | The list of all tenants, for the info page.
    tenants :: Set TenantName,
    -- | The zuul config.
    config :: Config,
    -- | A list of error found when building the analysis.
    graphErrors :: [String]
  }
  deriving (Show, Generic)

-- | The main function to build the 'Analysis' .
analyzeConfig :: TenantsConfig -> Config -> Analysis
analyzeConfig (Zuul.Tenant.TenantsConfig tenantsConfig) config =
  runIdentity (execStateT go (Analysis Algebra.Graph.empty Algebra.Graph.empty mempty mempty allTenants config mempty))
  where
    allTenants = Set.fromList $ Map.keys tenantsConfig
    -- All the default base jobs defined by the tenants
    -- Given:
    -- - tenant1, tenant2 default base job is 'base'
    -- - tenant3 default base job is 'base-minimal'
    -- Then: baseJobs = [(base, [tenant1, tenant2]), (base-minimal, [tenant3])]
    baseJobs :: [(JobName, Set TenantName)]
    baseJobs = Map.toList baseJobsMap
      where
        baseJobsMap :: Map JobName (Set TenantName)
        baseJobsMap = foldr insertTenant mempty (Map.toList tenantsConfig)
        insertTenant (tenantName, tenantConfig) =
          Map.insertWith Set.union tenantConfig.defaultParent (Set.singleton tenantName)

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
          | otherwise = case mapMaybe (setParentJob (loc, job)) baseJobs of
              [] -> error "This job is not attached to any tenant?!"
              xs -> xs
        setParentJob :: (ConfigLoc, Job) -> (JobName, Set TenantName) -> Maybe (ConfigLoc, Job)
        setParentJob (loc, job) (parent, tenants)
          -- The default base job is from other tenants
          | all (`notElem` loc.tenants) tenants = Nothing
          -- This job is the base job, we don't set it's parent
          | job.name == parent = Just (loc, job)
          -- We create a new job with the parent set to the list of tenants defining it
          | otherwise = Just (loc & (#tenants `set` tenants), job {parent = Just parent})

    go :: State Analysis ()
    go = do
      traverse_ goJob $ concat $ Map.elems allJobs
      traverse_ goNodeset $ concat $ Map.elems config.nodesets
      traverse_ goProject $ concat $ Map.elems config.projects
      traverse_ goProjectTemplate $ concat $ Map.elems config.projectTemplates
      traverse_ goPipeline $ concat $ Map.elems config.pipelines
    -- look for semaphore, secret, ...

    lookupTenant :: Ord a => Set TenantName -> a -> ConfigMap a b -> Maybe [(ConfigLoc, b)]
    lookupTenant tenants key cm = filterTenants =<< Map.lookup key cm
      where
        filterTenants xs = case filter (matchingTenant . fst) xs of
          [] -> Nothing
          xs' -> Just xs'
        matchingTenant :: ConfigLoc -> Bool
        matchingTenant loc = any (`elem` loc.tenants) tenants

    goPipeline :: (ConfigLoc, Pipeline) -> State Analysis ()
    goPipeline (loc, pipeline) = insertVertex $ mkVertex loc pipeline

    goProjectPipeline :: ConfigLoc -> Vertex -> Set ProjectPipeline -> State Analysis ()
    goProjectPipeline loc src projectPipelines = do
      forM_ projectPipelines $ \pipeline -> do
        let psrc = Vertex (VPipeline pipeline.name) loc.tenants
        case lookupTenant loc.tenants pipeline.name config.pipelines of
          Just xs -> goFeedState src xs
          Nothing -> #graphErrors %= (("Can't find : " <> show pipeline) :)
        forM_ (filter (\j -> from j /= JobName "noop") pipeline.jobs) $ \pJob -> do
          case lookupTenant loc.tenants (from pJob) config.jobs of
            Just xs -> do
              -- Create link with the project
              goFeedState src xs
              -- Create link with the pipeline
              goFeedState psrc xs
            Nothing -> #graphErrors %= (("Can't find : " <> show (into @JobName pJob)) :)
          case pJob of
            PJName _ -> pure ()
            PJJob job -> goJob (loc, job)

    goProject :: (ConfigLoc, Project) -> State Analysis ()
    goProject (loc, project) = do
      let src = mkVertex loc project
      insertVertex src
      forM_ project.templates $ \templateName -> do
        case lookupTenant loc.tenants templateName config.projectTemplates of
          Just xs -> goFeedState src xs
          Nothing -> #graphErrors %= (("Can't find : " <> show templateName) :)
      goProjectPipeline loc src project.pipelines

    goProjectTemplate :: (ConfigLoc, ProjectTemplate) -> State Analysis ()
    goProjectTemplate (loc, tmpl) = do
      let src = mkVertex loc tmpl
      insertVertex src
      goProjectPipeline loc src tmpl.pipelines

    goNodeset :: (ConfigLoc, Nodeset) -> State Analysis ()
    goNodeset (loc, nodeset) = do
      let src = mkVertex loc nodeset
      insertVertex src
      forM_ nodeset.labels $ \label -> do
        let dst = mkVertex loc label
        insertVertex dst
        feedState (src, dst)

    goJob :: (ConfigLoc, Job) -> State Analysis ()
    goJob (loc, job) = do
      let src = mkVertex loc job
      insertVertex src
      -- look for nodeset location
      case job.nodeset of
        Just (JobNodeset nodeset) -> case lookupTenant loc.tenants nodeset config.nodesets of
          Just xs -> goFeedState src xs
          Nothing -> #graphErrors %= (("Can't find : " <> show nodeset) :)
        Just (JobAnonymousNodeset nodeLabels) -> do
          forM_ nodeLabels $ \nodeLabel -> do
            let dst = mkVertex loc nodeLabel
            insertVertex dst
            feedState (src, dst)
        _ -> pure ()

      -- look for job parent
      case job.parent of
        Just parent -> do
          case lookupTenant loc.tenants parent allJobs of
            Just xs -> goFeedState src xs
            Nothing -> #graphErrors %= (("Can't find : " <> show parent) :)
        Nothing -> pure ()

      -- look for job dependencies
      forM_ job.dependencies $ \dJob' -> do
        case lookupTenant loc.tenants dJob' allJobs of
          -- TODO: look in priority for PJJob defined in the same loc
          Just xs -> goFeedState src xs
          Nothing -> #graphErrors %= (("Can't find : " <> show dJob') :)

    goFeedState :: From a VertexName => Vertex -> [(ConfigLoc, a)] -> State Analysis ()
    goFeedState src = traverse_ (\(loc, dst) -> feedState (src, mkVertex loc dst))

    feedState :: (Vertex, Vertex) -> State Analysis ()
    feedState (a, b) = do
      #configRequireGraph %= Algebra.Graph.overlay (Algebra.Graph.edge a b)
      #configDependsOnGraph %= Algebra.Graph.overlay (Algebra.Graph.edge b a)

    insertVertex :: Vertex -> State Analysis ()
    insertVertex v = do
      #vertices %= Set.insert v
      #names %= Map.insertWith Set.union v.name v.tenants
