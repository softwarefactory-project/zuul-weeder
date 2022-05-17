module ZuulWeeder.Graph
  ( Analysis (..),
    ConfigGraph,
    ConfigVertex (..),
    VertexType (..),
    Names,
    Vertex,
    pattern VNodeLabel,
    pattern VJob,
    pattern VProjectPipeline,
    pattern VNodeset,
    pattern VPipeline,
    pattern VProjectTemplate,
    pattern VQueue,
    pattern VSemaphore,

    -- * Graph functions
    analyzeConfig,
    findReachable,
    filterTenant,
  )
where

import Algebra.Graph qualified
import Algebra.Graph.ToGraph qualified
import Data.Map qualified as Map
import Data.Set qualified as Set
import Zuul.ConfigLoader
import Zuul.Tenant
import ZuulWeeder.Prelude

-- | The graph vertex
-- Node label names are added to the raw zuul config element so that they can be searched independently.
data ConfigVertex
  = ZuulConfigVertex ZuulConfigElement
  | NodeLabelVertex NodeLabelName
  deriving (Eq, Ord, Show, Generic, Hashable)

instance From ConfigVertex ConfigName where
  from (ZuulConfigVertex zce) = from zce
  from (NodeLabelVertex (NodeLabelName n)) = ConfigName n

data VertexType
  = ZuulConfigVertexType ZuulConfigType
  | NodeLabelVertexType
  deriving (Eq, Ord, Show)

instance From ConfigVertex VertexType where
  from cv = case cv of
    ZuulConfigVertex zce -> ZuulConfigVertexType (from zce)
    NodeLabelVertex _ -> NodeLabelVertexType

instance From VertexType Text where
  from vt = case vt of
    ZuulConfigVertexType zct -> from zct
    NodeLabelVertexType -> "label"

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

pattern VQueue :: Zuul.ConfigLoader.Queue -> ConfigVertex
pattern VQueue x = ZuulConfigVertex (ZQueue x)

pattern VSemaphore :: Zuul.ConfigLoader.Semaphore -> ConfigVertex
pattern VSemaphore x = ZuulConfigVertex (ZSemaphore x)

{-# COMPLETE VNodeLabel, VJob, VProjectPipeline, VNodeset, VProjectTemplate, VPipeline, VQueue, VSemaphore #-}

type Vertex = (ConfigLoc, ConfigVertex)

type ConfigGraph = Algebra.Graph.Graph Vertex

findReachable :: Vertex -> ConfigGraph -> Set.Set Vertex
findReachable v = Set.fromList . filter (/= v) . Algebra.Graph.ToGraph.reachable v

filterTenant :: TenantName -> ConfigGraph -> ConfigGraph
filterTenant tenant = Algebra.Graph.induce (\(loc, _) -> tenant `elem` loc.tenants)

type Names = Map ConfigName [Vertex]

data Analysis = Analysis
  { configRequireGraph :: ConfigGraph,
    configDependsOnGraph :: ConfigGraph,
    -- | The list of all the configuration names and their location
    names :: Names,
    config :: Config,
    graphErrors :: [String]
  }
  deriving (Show, Generic)

analyzeConfig :: TenantsConfig -> Config -> Analysis
analyzeConfig (Zuul.Tenant.TenantsConfig tenantsConfig) config =
  runIdentity $ execStateT go (Analysis Algebra.Graph.empty Algebra.Graph.empty mempty config mempty)
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
        -- TODO: handle templates
        forM_ pPipeline.pipelines $ \pipeline -> do
          case lookupTenant loc.tenants pipeline.name config.pipelines of
            Just xs ->
              forM_ xs $ \(loc', pipeline') -> do
                feedState ((loc, VProjectPipeline pPipeline), (loc', VPipeline pipeline'))
            Nothing -> #graphErrors %= (("Can't find : " <> show pipeline) :)
          forM_ pipeline.jobs $ \pJob -> do
            case pJob of
              pj@(PJName jobName) -> do
                case lookupTenant loc.tenants jobName config.jobs of
                  Just xs -> do
                    -- TODO: filter using tenant config
                    forM_ xs $ \(loc'', job) -> do
                      feedState ((loc, VProjectPipeline pPipeline), (loc'', VJob job))
                  Nothing -> #graphErrors %= (("Can't find : " <> show pj) :)
              PJJob _job -> do
                -- TODO: handle inline job
                pure ()

    -- TODO: connect to job

    goNodesets :: [(ConfigLoc, Nodeset)] -> State Analysis ()
    goNodesets nodesets = do
      forM_ nodesets $ \(loc, nodeset) -> do
        insertName nodeset (loc, VNodeset nodeset)
        forM_ nodeset.labels $ \label -> do
          insertName label (loc, VNodeLabel label)
          feedState ((loc, VNodeset nodeset), (loc, VNodeLabel label))

    goJobs :: [(ConfigLoc, Job)] -> State Analysis ()
    goJobs jobs = do
      -- TODO: filter using tenant config
      forM_ jobs $ \(loc, job) -> do
        insertName job (loc, VJob job)
        -- look for nodeset location
        case job.nodeset of
          Just (JobNodeset nodeset) -> case lookupTenant loc.tenants nodeset config.nodesets of
            Just xs ->
              forM_ xs $ \(loc', ns) -> feedState ((loc, VJob job), (loc', VNodeset ns))
            Nothing -> #graphErrors %= (("Can't find : " <> show nodeset) :)
          Just (JobAnonymousNodeset nodeLabels) -> do
            forM_ nodeLabels $ \nodeLabel -> do
              insertName nodeLabel (loc, VNodeLabel nodeLabel)
              feedState ((loc, VJob job), (loc, VNodeLabel nodeLabel))
          _ -> pure ()
        -- look for job parent
        case job.parent of
          Just parent -> do
            case lookupTenant loc.tenants parent allJobs of
              Just xs ->
                forM_ xs $ \(loc', pj) -> feedState ((loc, VJob job), (loc', VJob pj))
              Nothing -> #graphErrors %= (("Can't find : " <> show parent) :)
          Nothing -> pure ()
        -- look for job dependencies
        forM_ job.dependencies $ \dJob' -> do
          case lookupTenant loc.tenants dJob' allJobs of
            Just xs ->
              -- TODO: look in priority for PJJob defined in the same loc
              forM_ xs $ \(loc', dJob) -> feedState ((loc, VJob job), (loc', VJob dJob))
            Nothing -> #graphErrors %= (("Can't find : " <> show dJob') :)

    feedState :: (Vertex, Vertex) -> State Analysis ()
    feedState (a, b) = do
      #configRequireGraph %= Algebra.Graph.overlay (Algebra.Graph.edge a b)
      #configDependsOnGraph %= Algebra.Graph.overlay (Algebra.Graph.edge b a)

    insertName k v = do
      #names %= Map.insertWith mappend (from k) [v]
