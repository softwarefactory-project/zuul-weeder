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
-- The config analysis results looks like this:
-- ![demo-graph](doc/demo-graph.svg)
module ZuulWeeder.Graph (
  ConfigGraph,
  Analysis (..),
  AnalysisStatus (..),
  newAnalysisStatus,
  Vertex (..),
  VertexType (..),
  VertexName (..),
  analyzeConfig,
  findReachable,
  findReachableForest,
)
where

import Algebra.Graph qualified
import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Algorithm qualified
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
  { name :: VertexName
  -- ^ The vertex identifier
  , tenants :: Set TenantName
  -- ^ The list of tenants using that vertex
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

-- WARNING: when adding new VertexName, you need to update the FromHttpApiData VertexTypeUrl instance in the UI module.

-- | A Vertex can be a raw zuul config element, or a custom element added through analysis
data VertexName
  = -- | An abstract job
    VAbstractJob JobName
  | -- | A job
    VJob JobName
  | -- | A secret
    VSecret SecretName
  | -- | A semaphore
    VSemaphore SemaphoreName
  | -- | A nodeset
    VNodeset NodesetName
  | -- | A node label
    VNodeLabel NodeLabelName
  | -- | A queue
    VQueue QueueName
  | -- | A project configuration
    VProject CanonicalProjectName
  | -- | A project regex
    VProjectRegex ProjectRegex
  | -- | A project template
    VProjectTemplate ProjectTemplateName
  | -- | A pipeline
    VPipeline PipelineName
  | -- | A project pipeline config
    VProjectPipeline PipelineName CanonicalProjectName
  | -- | A project regex config
    VRegexPipeline PipelineName ProjectRegex
  | -- | A template pipeline config
    VTemplatePipeline PipelineName ProjectTemplateName
  | -- | A repository (containing zuul config objects)
    VRepository CanonicalProjectName
  | -- | A pipeline trigger
    VTrigger ConnectionName
  | -- | A pipeline reporter
    VReporter ConnectionName
  deriving (Eq, Ord, Show, Generic, Hashable)

data VertexType
  = VAbstractJobT
  | VJobT
  | VSemaphoreT
  | VSecretT
  | VNodesetT
  | VNodeLabelT
  | VQueueT
  | VPipelineT
  | VProjectT
  | VProjectPipelineT
  | VProjectRegexT
  | VRegexPipelineT
  | VProjectTemplateT
  | VTemplatePipelineT
  | VRepositoryT
  | VTriggerT
  | VReporterT
  deriving (Eq, Ord, Enum, Bounded)

instance From VertexName VertexType where
  from = \case
    VAbstractJob _ -> VAbstractJobT
    VJob _ -> VJobT
    VSemaphore _ -> VSemaphoreT
    VSecret _ -> VSecretT
    VQueue _ -> VQueueT
    VProject _ -> VProjectT
    VProjectRegex _ -> VProjectRegexT
    VNodeset _ -> VNodesetT
    VProjectTemplate _ -> VProjectTemplateT
    VPipeline _ -> VPipelineT
    VNodeLabel _ -> VNodeLabelT
    VProjectPipeline _ _ -> VProjectPipelineT
    VRegexPipeline _ _ -> VRegexPipelineT
    VTemplatePipeline _ _ -> VTemplatePipelineT
    VRepository _ -> VRepositoryT
    VTrigger _ -> VTriggerT
    VReporter _ -> VReporterT

instance From VertexName Text where
  from = \case
    VAbstractJob (JobName n) -> n
    VJob (JobName n) -> n
    VSecret (SecretName n) -> n
    VSemaphore (SemaphoreName n) -> n
    VNodeset (NodesetName n) -> n
    VNodeLabel (NodeLabelName n) -> n
    VQueue (QueueName n) -> n
    VProject (CanonicalProjectName (ProviderName p) (ProjectName n)) -> p <> "/" <> n
    VProjectRegex (ProjectRegex n) -> n
    VProjectTemplate (ProjectTemplateName n) -> n
    VPipeline (PipelineName n) -> n
    VRegexPipeline (PipelineName v) (ProjectRegex n) -> v <> ":" <> n
    VProjectPipeline (PipelineName v) cp -> v <> ":" <> from cp
    VTemplatePipeline (PipelineName v) (ProjectTemplateName n) -> v <> ":" <> n
    VRepository (CanonicalProjectName (ProviderName p) (ProjectName n)) -> p <> "/" <> n
    VTrigger (ConnectionName n) -> n
    VReporter (ConnectionName n) -> n

instance From Job VertexName where
  from job
    | job.abstract == Just True = VAbstractJob job.name
    | otherwise = VJob job.name

instance From CanonicalProjectName VertexName where
  from = VProject

instance From SecretName VertexName where
  from = VSecret

instance From ProjectRegex VertexName where
  from = VProjectRegex

instance From SemaphoreName VertexName where
  from = VSemaphore

instance From QueueName VertexName where
  from = VQueue

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
findReachable xs graph = Set.fromList . filter (/= v) $ Algebra.Graph.ToGraph.dfs graph (NE.toList xs)
 where
  v = NE.head xs

-- | Return the forest of reachable 'Vertex'
findReachableForest ::
  Maybe (Set TenantName) ->
  -- | The list of 'Vertex' to search
  NonEmpty Vertex ->
  -- | The graph to search in
  AM.AdjacencyMap Vertex ->
  -- | The forest
  Forest VertexName
findReachableForest baseScope xs graph = concatMap goRoot $ Algebra.Graph.AdjacencyMap.Algorithm.bfsForest graph vertices
 where
  vertices = NE.toList xs
  goRoot (Node top child) = concatMap (go scope) child
   where
    scope = case baseScope of
      Just tenants -> tenants `Set.intersection` top.tenants
      Nothing -> top.tenants

  go :: Set TenantName -> Tree Vertex -> [Tree VertexName]
  go scope tree@(Node root _)
    | Set.null newScope = []
    | otherwise = go' newScope tree
   where
    newScope = scope `Set.intersection` root.tenants

  go' :: Set TenantName -> Tree Vertex -> [Tree VertexName]
  go' scope (Node root childs) = [Node root.name (concatMap (go scope) childs)]

-- | The config analysis result used by the "ZuulWeeder.UI" module.
data Analysis = Analysis
  { dependencyGraph :: ConfigGraph
  -- ^ The requirements graph, e.g. job requires nodeset.
  , dependentGraph :: ConfigGraph
  -- ^ The dependents graph, e.g. nodeset allows job.
  , dependencyMap :: AM.AdjacencyMap Vertex
  , dependentMap :: AM.AdjacencyMap Vertex
  , repositoryContent :: Map CanonicalProjectName (Set Vertex)
  -- ^ The link between repositories and configuration objects.
  , vertices :: Set Vertex
  -- ^ The list of vertex, used for displaying search result.
  , names :: Map VertexName (Set TenantName)
  -- ^ A map of all the names and their matching tenants, used for searching.
  , graphErrors :: [String]
  -- ^ A list of error found when building the analysis.
  , config :: Config
  -- ^ The zuul config.
  }
  deriving (Show, Generic)

data AnalysisStatus = AnalysisStatus
  { loadingError :: Maybe Text
  , refreshing :: Bool
  , analysis :: Analysis
  }
  deriving (Show, Generic)

newAnalysisStatus :: Analysis -> AnalysisStatus
newAnalysisStatus = AnalysisStatus Nothing False

defaultAnalysis :: Config -> Analysis
defaultAnalysis = Analysis Algebra.Graph.empty Algebra.Graph.empty AM.empty AM.empty mempty mempty mempty mempty

-- | The main function to build the 'Analysis' .
analyzeConfig :: TenantsConfig -> Config -> Analysis
analyzeConfig (Zuul.Tenant.TenantsConfig tenantsConfig) config =
  let result = runIdentity (execStateT go (defaultAnalysis config))
      depMap = Algebra.Graph.ToGraph.toAdjacencyMap result.dependencyGraph
      dependentMap = Algebra.Graph.ToGraph.toAdjacencyMap result.dependentGraph
   in result & (#dependencyMap `set` depMap) . (#dependentMap `set` dependentMap)
 where
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
  --   [ (job1, [ (loc, job1 {parent = Just base})
  --            , (loc, job1 {parent = Just base-minimal}) ])
  --   , (job2, [ (loc, job2) ]) ]
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
      | otherwise = Just (loc & (#tenants `set` (Set.intersection loc.tenants tenants)), job {parent = Just parent})

  go :: State Analysis ()
  go = do
    traverse_ goCanonicalProjects $ Map.toList config.canonicalProjects
    traverse_ goJob $ concat $ Map.elems allJobs
    traverse_ goNodeset $ concat $ Map.elems config.nodesets
    traverse_ goProject $ concatMap projectList (Map.toList config.projects)
    traverse_ goProjectRegex $ concat $ Map.elems config.projectRegexs
    traverse_ goProjectTemplate $ concat $ Map.elems config.projectTemplates
    traverse_ goPipeline $ concat $ Map.elems config.pipelines
    traverse_ goInsert $ concat $ Map.elems config.secrets
    traverse_ goInsert $ concat $ Map.elems config.semaphores
    traverse_ goInsert $ concat $ Map.elems config.queues
   where
    projectList :: (CanonicalProjectName, [(ConfigLoc, Project)]) -> [(CanonicalProjectName, (ConfigLoc, Project))]
    projectList (a, b) = (a,) <$> b

  goInsert (loc, obj) = do
    let vertex = mkVertex loc obj
    insertVertex loc vertex

  -- get the list of vertex matching a given name and set of tenants
  lookupTenant :: (Ord a, From b VertexName) => Set TenantName -> a -> ConfigMap a b -> Maybe (Set Vertex)
  lookupTenant tenants key cm = filterTenants =<< Map.lookup key cm
   where
    filterTenants xs = case filter (matchingTenant . fst) xs of
      [] -> Nothing
      xs' -> Just (toVertices xs')
    matchingTenant :: ConfigLoc -> Bool
    matchingTenant loc = any (`elem` loc.tenants) tenants
    toVertices = Set.fromList . map (\(loc, dst) -> mkVertex loc dst)

  goPipeline :: (ConfigLoc, Pipeline) -> State Analysis ()
  goPipeline (loc, pipeline) = do
    let vPipeline = mkVertex loc pipeline
    insertVertex loc vPipeline
    forM_ pipeline.triggers $ \(PipelineTrigger trigger) -> do
      let vTrigger = Vertex (VTrigger trigger) loc.tenants
      vPipeline `connect` vTrigger
      insertVertex loc vTrigger
    forM_ pipeline.reporters $ \(PipelineReporter trigger) -> do
      let vReporter = Vertex (VReporter trigger) loc.tenants
      vPipeline `connect` vReporter
      insertVertex loc vReporter

  goPipelineConfig :: ConfigLoc -> Vertex -> (PipelineName -> VertexName) -> ProjectPipeline CanonicalProjectName -> State Analysis ()
  goPipelineConfig loc vProject mk pipeline = do
    -- pipeline config is a list of jobs attached to a project pipeline
    let vPipelineConfig = Vertex (mk pipeline.name) loc.tenants
    insertVertex loc vPipelineConfig

    -- pipeline is the global pipeline object
    let vPipeline = Vertex (VPipeline pipeline.name) loc.tenants

    vProject `connect` vPipelineConfig
    vPipelineConfig `connect` vPipeline

    -- handle pipeline jobs
    forM_ (filter (\j -> from j /= JobName "noop") pipeline.jobs) $ \pJob -> do
      case lookupTenant loc.tenants (from pJob) config.jobs of
        Just jobs -> do
          traverse_ (\job -> vPipelineConfig `allows` job) jobs
          vPipelineConfig `connects` jobs
        Nothing -> #graphErrors %= (("Can't find : " <> show (into @JobName pJob)) :)
      case pJob of
        PJName _ ->
          -- job referenced by name does not need to be processed.
          pure ()
        PJJob job ->
          -- job that are overriden are handled as a new job.
          goJob (loc, job)

  goQueue :: ConfigLoc -> Vertex -> Maybe QueueName -> State Analysis ()
  goQueue loc src = \case
    Just queue -> case lookupTenant loc.tenants queue config.queues of
      Just queues -> src `connects` queues
      Nothing -> #graphErrors %= (("Can't find : " <> show queue) :)
    Nothing -> pure ()

  goProject :: (CanonicalProjectName, (ConfigLoc, Project)) -> State Analysis ()
  goProject (projectName, (loc, project)) = do
    let vProject = mkVertex loc projectName
    insertVertex loc vProject

    -- handle queue and templates
    goProjectConfig loc project vProject

    -- handle project pipeline configs
    traverse_ (goPipelineConfig loc vProject (flip VProjectPipeline projectName)) project.pipelines

  goProjectConfig loc project v = do
    -- handle queues
    goQueue loc v project.queue

    forM_ project.templates $ \templateName -> do
      case lookupTenant loc.tenants templateName config.projectTemplates of
        Just templates -> v `connects` templates
        Nothing -> #graphErrors %= (("Can't find : " <> show templateName) :)

  goProjectRegex :: (ConfigLoc, Project) -> State Analysis ()
  goProjectRegex (loc, project) = do
    let regex = into @ProjectRegex project.name
    let vProjectRegex = mkVertex loc regex
    insertVertex loc vProjectRegex

    -- handle templates
    goProjectConfig loc project vProjectRegex

    -- handle project pipeline configs
    traverse_ (goPipelineConfig loc vProjectRegex (flip VRegexPipeline regex)) project.pipelines

  goProjectTemplate :: (ConfigLoc, ProjectTemplate) -> State Analysis ()
  goProjectTemplate (loc, tmpl) = do
    let src = mkVertex loc tmpl
    insertVertex loc src

    -- handle queues
    goQueue loc src tmpl.queue

    -- handle template pipeline config
    traverse_ (goPipelineConfig loc src (flip VTemplatePipeline tmpl.name)) tmpl.pipelines

  goNodeset :: (ConfigLoc, Nodeset) -> State Analysis ()
  goNodeset (loc, nodeset) = do
    let src = mkVertex loc nodeset
    insertVertex loc src

    -- handle labels
    forM_ nodeset.labels $ \label -> do
      let dst = mkVertex loc label
      insertVertex loc dst
      src `connect` dst

  goJob :: (ConfigLoc, Job) -> State Analysis ()
  goJob (loc, job) = do
    let vJob = mkVertex loc job
    insertVertex loc vJob

    -- handle nodesets and anonymous node label
    case job.nodeset of
      Just (JobNodeset nodeset) -> case lookupTenant loc.tenants nodeset config.nodesets of
        Just vNodesets -> vJob `connects` vNodesets
        Nothing -> #graphErrors %= (("Can't find : " <> show nodeset) :)
      Just (JobAnonymousNodeset nodeLabels) -> do
        forM_ nodeLabels $ \nodeLabel -> do
          let vLabel = mkVertex loc nodeLabel
          insertVertex loc vLabel
          vJob `connect` vLabel
      Nothing -> pure ()

    -- handle job parent
    case job.parent of
      Just parent -> do
        case lookupTenant loc.tenants parent allJobs of
          Just vParentJobs -> vJob `connects` vParentJobs
          Nothing -> #graphErrors %= (("Can't find : " <> show parent) :)
      Nothing -> pure ()

    -- handle job dependencies
    forM_ (concat job.dependencies) $ \dJob -> do
      case lookupTenant loc.tenants dJob allJobs of
        Just vDependencyJobs -> vJob `connects` vDependencyJobs
        Nothing -> #graphErrors %= (("Can't find : " <> show dJob) :)

    -- handle job required-projects
    forM_ (concat job.requiredProjects) $ \dProj ->
      case Map.lookup dProj config.canonicalProjects of
        Just tenants -> vJob `connect` Vertex (from dProj) tenants
        Nothing -> #graphErrors %= (("Can't find : " <> show dProj) :)

    -- handle job roles
    forM_ (concat job.rolesProjects) $ \rolesProj ->
      case Map.lookup rolesProj config.canonicalProjects of
        Just tenants -> vJob `connect` Vertex (from rolesProj) tenants
        Nothing -> #graphErrors %= (("Can't find : " <> show rolesProj) :)

    -- handle job secrets
    forM_ (concat job.secrets) $ \secret -> do
      case lookupTenant loc.tenants secret config.secrets of
        Just vSecrets -> vJob `connects` vSecrets
        Nothing -> #graphErrors %= (("Can't find : " <> show secret) :)

    -- handle job semaphores
    forM_ (concat job.semaphores) $ \semaphore -> do
      case lookupTenant loc.tenants semaphore config.semaphores of
        Just vSemaphores -> vJob `connects` vSemaphores
        Nothing -> #graphErrors %= (("Can't find : " <> show semaphore) :)

  -- connect two vertices: src and dst, where src requires dst and dst allows src.
  -- see https://english.stackexchange.com/questions/248642/inverse-of-dependency
  --
  -- This function is used for most elements, e.g.: job <-> nodeset <-> label
  --
  -- However there are a few exceptions where the relationship is restricted:
  --
  -- - Project pipelines are not directly attached to avoid un-necessary interconnections.
  --   For example, instead of:
  --
  --     project1 <-> check <-> job1
  --     project2 <-> check <-> job2
  --
  --   We don't connect through the global check to avoid having the job2 to be a requirement of project1:
  --
  --     project1 <-> project1:check <-> job1
  --        check <-> project1:check
  --     project2 <-> project2:check <-> job2
  --        check <-> project2:check
  --
  --   That way there is no connection between job2 and project1
  --
  -- - Project containing configuration allows the element, but the project is not a requirements.
  connect :: Vertex -> Vertex -> State Analysis ()
  connect src dst = do
    src `requires` dst
    dst `allows` src

  -- see https://english.stackexchange.com/questions/248642/inverse-of-dependency
  -- For a nodeset `foo` that has a label `bar`:
  --   `foo` is a dependent of `bar`, because `foo` requires `bar`
  --   `bar` is a dependency of `foo`, because `bar` allows `foo`
  requires, allows :: Vertex -> Vertex -> State Analysis ()
  a `requires` b = #dependencyGraph %= Algebra.Graph.overlay (Algebra.Graph.edge a b)
  a `allows` b = #dependentGraph %= Algebra.Graph.overlay (Algebra.Graph.edge a b)

  -- connects one vertex to a set of vertex, such as the object founds in different tenants.
  connects :: Vertex -> Set Vertex -> State Analysis ()
  connects src = traverse_ (connect src)

  -- insert a vertex and connect the config loc to the vertex.
  insertVertex :: ConfigLoc -> Vertex -> State Analysis ()
  insertVertex loc v = do
    #vertices %= Set.insert v
    #names %= Map.insertWith Set.union v.name v.tenants

    registerObject loc.project v

  registerObject :: CanonicalProjectName -> Vertex -> State Analysis ()
  registerObject proj v
    | isProjectPipeline = pure ()
    | otherwise = do
        #repositoryContent %= Map.insertWith Set.union proj (Set.singleton v)
   where
    isProjectPipeline = case v.name of
      VProjectPipeline {} -> True
      VRegexPipeline {} -> True
      VTemplatePipeline {} -> True
      _ -> False

  goCanonicalProjects :: (CanonicalProjectName, Set TenantName) -> State Analysis ()
  goCanonicalProjects (repo, tenants) = do
    let vRepo = VRepository repo
    #vertices %= Set.insert (Vertex vRepo tenants)
    #names %= Map.insertWith Set.union vRepo tenants
