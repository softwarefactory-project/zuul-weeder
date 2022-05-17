module ZuulWeeder.Graph
  ( Analysis (..),
    ConfigGraph,
    Vertex (..),
    VertexName (..),
    vertexTypeName,

    -- * Graph functions
    analyzeConfig,
    findReachable,
    filterTenant,
    mkVertex,
  )
where

import Algebra.Graph qualified
import Algebra.Graph.ToGraph qualified
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Lazy.Builder qualified as TB
import Zuul.ConfigLoader
import Zuul.Tenant
import ZuulWeeder.Prelude

-- | The graph vertex
data Vertex = Vertex
  { tenants :: [TenantName],
    name :: VertexName
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

instance Display Vertex where
  displayBuilder v =
    TB.fromText (Text.pack . show $ v.name)

-- | A Vertex can be a raw zuul config element, or a custom element added through analysis
data VertexName
  = VJob JobName
  | VNodeset NodesetName
  | VNodeLabel NodeLabelName
  | VProjectPipeline Project
  | VProjectTemplate ProjectTemplateName
  | VPipeline PipelineName
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | A text representation of a vertex type, useful for /object url piece
vertexTypeName :: VertexName -> Text
vertexTypeName = \case
  VJob _ -> "job"
  VNodeset _ -> "nodeset"
  VNodeLabel _ -> "label"
  VProjectPipeline _ -> "todo-project-pipeline"
  VProjectTemplate _ -> "project-template"
  VPipeline _ -> "pipeline"

instance From VertexName Text where
  from = \case
    VJob (JobName n) -> n
    VNodeset (NodesetName n) -> n
    VNodeLabel (NodeLabelName n) -> n
    VProjectPipeline _ -> "todo-pp"
    VProjectTemplate (ProjectTemplateName n) -> n
    VPipeline (PipelineName n) -> n

instance From Job VertexName where
  from job = VJob job.name

instance From ProjectPipeline VertexName where
  from pp = VProjectPipeline pp.name

instance From Pipeline VertexName where
  from p = VPipeline p.name

instance From Nodeset VertexName where
  from p = VNodeset p.name

instance From NodeLabelName VertexName where
  from = VNodeLabel

mkVertex :: From a VertexName => ConfigLoc -> a -> Vertex
mkVertex loc x = Vertex loc.tenants (from x)

type ConfigGraph = Algebra.Graph.Graph Vertex

findReachable :: Vertex -> ConfigGraph -> Set.Set Vertex
findReachable v = Set.fromList . filter (/= v) . Algebra.Graph.ToGraph.reachable v

filterTenant :: TenantName -> ConfigGraph -> ConfigGraph
filterTenant tenant = Algebra.Graph.induce $ \vertex -> tenant `elem` vertex.tenants

data Analysis = Analysis
  { configRequireGraph :: ConfigGraph,
    configDependsOnGraph :: ConfigGraph,
    vertices :: Set Vertex,
    config :: Config,
    graphErrors :: [String]
  }
  deriving (Show, Generic)

analyzeConfig :: TenantsConfig -> Config -> Analysis
analyzeConfig (Zuul.Tenant.TenantsConfig tenantsConfig) config =
  runIdentity (execStateT go (Analysis Algebra.Graph.empty Algebra.Graph.empty mempty config mempty))
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
          | otherwise = case mapMaybe (setParentJob (loc, job)) baseJobs of
              [] -> error "This job is not attached to any tenant?!"
              xs -> xs
        setParentJob :: (ConfigLoc, Job) -> (JobName, [TenantName]) -> Maybe (ConfigLoc, Job)
        setParentJob (loc, job) (parent, tenants)
          -- The default base job is from other tenants
          | all (`notElem` loc.tenants) tenants = Nothing
          -- This job is the base job, we don't set it's parent
          | job.name == parent = Just (loc, job)
          -- We create a new job with the parent set to the list of tenants defining it
          | otherwise = Just (loc & (#tenants `set` tenants), job {parent = Just parent})

    go :: State Analysis ()
    go = do
      goJobs $ concat $ Map.elems allJobs
      goNodesets $ concat $ Map.elems config.nodesets
      goProjectPipelines $ concat $ Map.elems config.projectPipelines
    -- look for semaphore, secret, ...

    lookupTenant :: Ord a => [TenantName] -> a -> ConfigMap a b -> Maybe [(ConfigLoc, b)]
    lookupTenant tenants key cm = filterTenants =<< Map.lookup key cm
      where
        filterTenants xs = case filter (matchingTenant . fst) xs of
          [] -> Nothing
          xs' -> Just xs'
        matchingTenant :: ConfigLoc -> Bool
        matchingTenant loc = any (`elem` loc.tenants) tenants

    goProjectPipelines :: [(ConfigLoc, ProjectPipeline)] -> State Analysis ()
    goProjectPipelines pPipelines = do
      -- TODO: filter using tenant config
      forM_ pPipelines $ \(loc, pPipeline) -> do
        let src = mkVertex loc pPipeline
        -- TODO: handle templates
        forM_ pPipeline.pipelines $ \pipeline -> do
          case lookupTenant loc.tenants pipeline.name config.pipelines of
            Just xs ->
              forM_ xs $ \(loc', pipeline') -> do
                feedState (src, mkVertex loc' pipeline')
            Nothing -> #graphErrors %= (("Can't find : " <> show pipeline) :)
          forM_ pipeline.jobs $ \pJob -> do
            case pJob of
              pj@(PJName jobName) -> do
                case lookupTenant loc.tenants jobName config.jobs of
                  Just xs -> do
                    -- TODO: filter using tenant config
                    forM_ xs $ \(loc'', job) -> do
                      feedState (src, mkVertex loc'' job)
                  Nothing -> #graphErrors %= (("Can't find : " <> show pj) :)
              PJJob _job -> do
                -- TODO: handle inline job
                pure ()

    -- TODO: connect to job

    goNodesets :: [(ConfigLoc, Nodeset)] -> State Analysis ()
    goNodesets nodesets = do
      forM_ nodesets $ \(loc, nodeset) -> do
        let src = mkVertex loc nodeset
        insertName src
        forM_ nodeset.labels $ \label -> do
          let dst = mkVertex loc label
          insertName dst
          feedState (src, dst)

    goJobs :: [(ConfigLoc, Job)] -> State Analysis ()
    goJobs jobs = do
      -- TODO: filter using tenant config
      forM_ jobs $ \(loc, job) -> do
        let src = mkVertex loc job
        insertName src
        -- look for nodeset location
        case job.nodeset of
          Just (JobNodeset nodeset) -> case lookupTenant loc.tenants nodeset config.nodesets of
            Just xs ->
              forM_ xs $ \(loc', ns) -> feedState (src, mkVertex loc' ns)
            Nothing -> #graphErrors %= (("Can't find : " <> show nodeset) :)
          Just (JobAnonymousNodeset nodeLabels) -> do
            forM_ nodeLabels $ \nodeLabel -> do
              let dst = mkVertex loc nodeLabel
              insertName dst
              feedState (src, dst)
          _ -> pure ()

        -- look for job parent
        case job.parent of
          Just parent -> do
            case lookupTenant loc.tenants parent allJobs of
              Just xs ->
                forM_ xs $ \(loc', pj) -> feedState (src, mkVertex loc' pj)
              Nothing -> #graphErrors %= (("Can't find : " <> show parent) :)
          Nothing -> pure ()

        -- look for job dependencies
        forM_ job.dependencies $ \dJob' -> do
          case lookupTenant loc.tenants dJob' allJobs of
            Just xs ->
              -- TODO: look in priority for PJJob defined in the same loc
              forM_ xs $ \(loc', dJob) -> feedState (src, mkVertex loc' dJob)
            Nothing -> #graphErrors %= (("Can't find : " <> show dJob') :)

    feedState :: (Vertex, Vertex) -> State Analysis ()
    feedState (a, b) = do
      #configRequireGraph %= Algebra.Graph.overlay (Algebra.Graph.edge a b)
      #configDependsOnGraph %= Algebra.Graph.overlay (Algebra.Graph.edge b a)

    insertName v = do
      #vertices %= Set.insert v
