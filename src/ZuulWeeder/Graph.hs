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
  deriving (Eq, Ord, Show, Generic)

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
findReachable v = Set.fromList . Algebra.Graph.ToGraph.reachable v

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
    -- look for semaphore, secret, ...

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
        insertName job (loc, VJob job)
        -- look for nodeset location
        case job.nodeset of
          Just (JobNodeset nodeset) -> case Map.lookup nodeset config.nodesets of
            Just xs ->
              -- TODO: filter the nodeset that are in the same tenant (and same branch?)
              forM_ xs $ \(loc', ns) -> feedState ((loc, VJob job), (loc', VNodeset ns))
            Nothing -> #graphErrors %= (("Can't find : " <> show nodeset) :)
          _ ->
            -- Ignore inlined nodeset
            pure ()
        -- look for job parent
        case job.parent of
          Just parent -> do
            case Map.lookup parent allJobs of
              Just xs ->
                -- TODO: only keep the jobs that are in the same tenant as the parent
                forM_ xs $ \(loc', pj) -> feedState ((loc, VJob job), (loc', VJob pj))
              Nothing -> #graphErrors %= (("Can't find : " <> show parent) :)
          Nothing -> pure ()
        -- look for job dependencies
        forM_ job.dependencies $ \dJob' -> do
          case Map.lookup dJob' allJobs of
            Just xs ->
              -- TODO: only keep the jobs that are in the same tenant
              -- TODO: look in priority for PJJob defined in the same loc
              forM_ xs $ \(loc', dJob) -> feedState ((loc, VJob job), (loc', VJob dJob))
            Nothing -> #graphErrors %= (("Can't find : " <> show dJob') :)

    feedState :: (Vertex, Vertex) -> State Analysis ()
    feedState (a, b) = do
      #configRequireGraph %= Algebra.Graph.overlay (Algebra.Graph.edge a b)
      #configDependsOnGraph %= Algebra.Graph.overlay (Algebra.Graph.edge b a)

    insertName k v = do
      #names %= Map.insertWith mappend (from k) [v]
