-- |
-- Module      : Zuul.ConfigLoader
-- Description : Parse configuration items
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
--
-- Maintainer  : tdecacqu@redhat.com, fboucher@redhat.com
-- Stability   : provisional
-- Portability : portable
--
-- This module contains the logic to decode configuration file content.
module Zuul.ConfigLoader
  ( -- * The resulting configuration
    ConfigMap,
    Config (..),
    ConfigError,
    loadConfig,
    mergeConfig,
    emptyConfig,
    TenantResolver,
    ConnectionUrlMap,

    -- * Test helper
    decodeConfig,
  )
where

import Data.Aeson (Object, Value (Array, Bool, Null, String))
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified as HM (keys, lookup, toList)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as V
import Zuul.Config
import Zuul.Tenant (TenantResolver (..))
import Zuul.ZooKeeper (ConfigError (..), ZKFile (..))
import ZuulWeeder.Prelude

-- | A config map contains the list of every variants associated with their location.
type ConfigMap a b = Map a [(ConfigLoc, b)]

-- | The global configuration map.
data Config = Config
  { -- | The jobs.
    jobs :: ConfigMap JobName Job,
    -- | The nodesets.
    nodesets :: ConfigMap NodesetName Nodeset,
    -- | The node labels.
    nodeLabels :: ConfigMap NodeLabelName NodeLabelName,
    -- | The projects.
    projects :: ConfigMap CanonicalProjectName Project,
    -- | The projects regexp.
    projectRegexs :: ConfigMap ProjectRegex Project,
    -- | The project-templates.
    projectTemplates :: ConfigMap ProjectTemplateName ProjectTemplate,
    -- | The pipelines.
    pipelines :: ConfigMap PipelineName Pipeline,
    -- | The secrets.
    secrets :: ConfigMap SecretName SecretName,
    -- | The queues.
    queues :: ConfigMap QueueName QueueName,
    -- | The semaphores.
    semaphores :: ConfigMap SemaphoreName SemaphoreName,
    -- | The pipeline triggers.
    triggers :: ConfigMap ConnectionName ConnectionName,
    -- | The pipeline reporters.
    reporters :: ConfigMap ConnectionName ConnectionName,
    -- | Configuration errors.
    configErrors :: [ConfigError],
    -- | The list of all tenants
    tenants :: Set TenantName
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Merge two configurations by assigning unique tenant names.
mergeConfig :: Config -> Config -> Config
mergeConfig c1 c2 =
  Config
    { jobs = c1.jobs `mergeMap` c2.jobs,
      nodesets = c1.nodesets `mergeMap` c2.nodesets,
      nodeLabels = c1.nodeLabels `mergeMap` c2.nodeLabels,
      projects = c1.projects `mergeMap` c2.projects,
      projectRegexs = c1.projectRegexs `mergeMap` c2.projectRegexs,
      projectTemplates = c1.projectTemplates `mergeMap` c2.projectTemplates,
      pipelines = c1.pipelines `mergeMap` c2.pipelines,
      secrets = c1.secrets `mergeMap` c2.secrets,
      queues = c1.queues `mergeMap` c2.queues,
      semaphores = c1.semaphores `mergeMap` c2.semaphores,
      triggers = c1.triggers `mergeMap` c2.triggers,
      reporters = c1.reporters `mergeMap` c2.reporters,
      configErrors = nub $ c1.configErrors <> c2.configErrors,
      tenants = newTenants
    }
  where
    -- The new tenants list with unique name
    newTenants = Set.union c1.tenants (Set.map renameTenant c2.tenants)

    mergeMap :: (Ord a, Eq b) => ConfigMap a b -> ConfigMap a b -> ConfigMap a b
    mergeMap = Map.unionWith mergeLocs

    mergeLocs :: (Eq b) => [(ConfigLoc, b)] -> [(ConfigLoc, b)] -> [(ConfigLoc, b)]
    mergeLocs cm1 cm2 = foldl' mergeConfigLoc [] $ cm1 <> (first renameLoc <$> cm2)
      where
        renameLoc :: ConfigLoc -> ConfigLoc
        renameLoc loc = loc & #tenants `set` Set.map renameTenant loc.tenants

    mergeConfigLoc :: Eq b => [(ConfigLoc, b)] -> (ConfigLoc, b) -> [(ConfigLoc, b)]
    mergeConfigLoc xs x@(loc2, obj2) = go False xs
      where
        -- The object was not found in c1
        go False [] = [x]
        -- The object was merged
        go True [] = []
        -- Check if the object can be merged
        go merged ((loc1, obj1) : rest)
          | obj1 == obj2 && loc1 `sameLoc` loc2 = (loc1 `mergeLoc` loc2, obj1) : go True rest
          | otherwise = (loc1, obj1) : go merged rest

        sameLoc :: ConfigLoc -> ConfigLoc -> Bool
        sameLoc l1 l2 =
          and
            [ l1.project == l2.project,
              l1.branch == l2.branch,
              l1.path == l2.path,
              l1.url == l2.url
            ]

        mergeLoc :: ConfigLoc -> ConfigLoc -> ConfigLoc
        mergeLoc l1 l2 = l1 & #tenants `set` Set.union l1.tenants l2.tenants

    -- The renameTenant create unique tenant names for c2 locations
    renameTenant :: TenantName -> TenantName
    renameTenant tenantName
      | tenantName `Set.member` commonTenants = case foldM mkTenantName tenantName ['1' .. '9'] of
          Left newName -> newName
          Right _ -> error $ "Can't create a new tenant name for " <> show tenantName
      | otherwise = tenantName
    commonTenants = Set.intersection c1.tenants c2.tenants

    -- Returns Left if the name is unique, otherwise returns Right
    mkTenantName :: TenantName -> Char -> Either TenantName TenantName
    mkTenantName oldName@(TenantName n) c
      | newName `Set.member` allTenants = Right oldName
      | otherwise = Left newName
      where
        newName = TenantName $ Text.snoc n c
    allTenants = Set.union c1.tenants c2.tenants

doUpdateTopConfig :: Monad m => TenantResolver -> ConfigLoc -> ZuulConfigElement -> StateT Config m ()
doUpdateTopConfig tr configLoc ze = case ze of
  ZJob baseJob -> do
    job <- doResolveJob baseJob
    #jobs %= insertConfig job.name job
  ZNodeset node -> do
    #nodesets %= insertConfig node.name node
    traverse_ (\v -> #nodeLabels %= insertConfig v v) $ Set.fromList node.labels
  ZProject baseProject -> do
    project <- doResolveProjectJob baseProject
    if isRegex project.name
      then #projectRegexs %= insertConfig (from project.name) project
      else case tr.resolveProject configLoc project.name of
        Just pn -> #projects %= insertConfig pn project
        Nothing -> #configErrors %= (AmbiguousName (from project.name) :)
  ZProjectTemplate baseTemplate -> do
    template <- doResolveProjectTemplateJob baseTemplate
    #projectTemplates %= insertConfig template.name template
  ZPipeline pipeline -> do
    #pipelines %= insertConfig pipeline.name pipeline
    traverse_ (\(PipelineTrigger v) -> #triggers %= insertConfig v v) pipeline.triggers
    traverse_ (\(PipelineReporter v) -> #reporters %= insertConfig v v) pipeline.reporters
  ZSecret secret -> #secrets %= insertConfig secret secret
  ZQueue queue -> #queues %= insertConfig queue queue
  ZSemaphore semaphore -> #semaphores %= insertConfig semaphore semaphore
  where
    doResolveProjectTemplateJob :: Monad m => BaseProjectTemplate ProjectName -> StateT Config m ProjectTemplate
    doResolveProjectTemplateJob pt = do
      pipelines <- mapMSet doResolveProjectPipeline pt.pipelines
      pure $ pt & #pipelines `set` pipelines

    doResolveProjectJob :: Monad m => BaseProject ProjectName -> StateT Config m Project
    doResolveProjectJob p = do
      pipelines <- mapMSet doResolveProjectPipeline p.pipelines
      pure $ p & #pipelines `set` pipelines

    doResolveProjectPipeline :: Monad m => ProjectPipeline ProjectName -> StateT Config m (ProjectPipeline CanonicalProjectName)
    doResolveProjectPipeline projectPipeline = do
      jobs <- mapM doResolvePipelineJob projectPipeline.jobs
      pure $ projectPipeline & #jobs `set` jobs

    doResolvePipelineJob :: Monad m => PipelineJob ProjectName -> StateT Config m (PipelineJob CanonicalProjectName)
    doResolvePipelineJob = \case
      PJName jobName -> pure $ PJName jobName
      PJJob job -> PJJob <$> doResolveJob job

    doResolveJob :: Monad m => BaseJob ProjectName -> StateT Config m Job
    doResolveJob job = do
      requiredProjects <- doResolveProjects job.requiredProjects
      pure $ job & #requiredProjects `set` requiredProjects

    doResolveProjects :: Monad m => Maybe [ProjectName] -> StateT Config m (Maybe [CanonicalProjectName])
    doResolveProjects (Just xs) = Just . catMaybes <$> traverse doResolveProject xs
    doResolveProjects Nothing = pure Nothing
    doResolveProject :: Monad m => ProjectName -> StateT Config m (Maybe CanonicalProjectName)
    doResolveProject pn = case tr.resolveProject configLoc pn of
      Just cpn -> pure (Just cpn)
      Nothing -> do
        #configErrors %= (AmbiguousName (from pn) :)
        pure Nothing

    isRegex (ProjectName n) = "^" `Text.isPrefixOf` n
    insertConfig k v = Map.insertWith mappend k [(configLoc, v)]

updateTopConfig :: TenantResolver -> BaseConfigLoc Void -> Decoder ZuulConfigElement -> StateT Config IO ()
updateTopConfig tr configLoc (Decoder (Right ze))
  | Set.null tenants = pure ()
  | otherwise = doUpdateTopConfig tr (configLoc & #tenants `set` tenants) ze
  where
    tenants = tr.resolveTenants configLoc (from ze)
updateTopConfig _ configLoc (Decoder (Left (e, v))) =
  #configErrors %= (DecodeError configLoc.path e v :)

-- | Low level helper to decode a config file into a list of 'ZuulConfigElement'.
decodeConfig :: (CanonicalProjectName, BranchName) -> Value -> [Decoder ZuulConfigElement]
decodeConfig (CanonicalProjectName (ProviderName providerName) (ProjectName projectName), _branch) zkJSONData =
  case zkJSONData of
    Array vec -> mapMaybe (sequence . getConfigElement) $ V.toList vec
    _ -> [decodeFail "Unexpected root data structure" zkJSONData]
  where
    getConfigElement :: Value -> Decoder (Maybe ZuulConfigElement)
    getConfigElement rootObj = do
      (k, obj) <- getObjectKey =<< decodeObject rootObj
      case k of
        "job" -> Just . ZJob <$> decodeJob obj
        "nodeset" -> Just . ZNodeset <$> decodeNodeset obj
        "project" -> Just . ZProject <$> decodeProject obj
        "project-template" -> Just . ZProjectTemplate <$> decodeProjectTemplate obj
        "pipeline" -> Just . ZPipeline <$> decodePipeline obj
        "queue" -> Just . ZQueue . QueueName <$> getName obj
        "semaphore" -> Just . ZSemaphore . SemaphoreName <$> getName obj
        "pragma" -> pure Nothing
        "secret" -> Just . ZSecret . SecretName <$> getName obj
        _ -> decodeFail "Unknown root object" (Object obj)

    decodePipeline :: Object -> Decoder Pipeline
    decodePipeline va = do
      name <- PipelineName <$> getName va
      triggers <- fmap PipelineTrigger <$> getPipelineConnections "trigger"
      reporters <-
        fmap PipelineReporter . concat
          <$> sequence
            [ getPipelineConnections "success",
              getPipelineConnections "failure"
            ]
      pure $ Pipeline {name, triggers, reporters}
      where
        getPipelineConnections key = case HM.lookup key va of
          Just v -> do
            obj <- decodeObject v
            pure $ ConnectionName . Data.Aeson.Key.toText <$> HM.keys obj
          Nothing -> pure []

    decodeJob :: Object -> Decoder (BaseJob ProjectName)
    decodeJob va = do
      name <- JobName <$> getName va
      decodeJobContent name va

    decodeJobContent :: JobName -> Object -> Decoder (BaseJob ProjectName)
    decodeJobContent name va = do
      BaseJob name
        <$> fmap toMaybe' decodeJobAbstract
        <*> decodeJobParent
        <*> decodeJobNodeset
        <*> fmap toMaybe (decodeAsList "branches" BranchName va)
        <*> fmap toMaybe decodeJobDependencies
        <*> fmap toMaybe decodeRequiredProjects
        <*> fmap toMaybe decodeSemaphores
        <*> fmap toMaybe decodeSecrets
      where
        toMaybe' :: Bool -> Maybe Bool
        toMaybe' = \case
          False -> Nothing
          True -> Just True

        toMaybe :: [a] -> Maybe [a]
        toMaybe = \case
          [] -> Nothing
          xs -> Just xs

        decodeJobAbstract :: Decoder Bool
        decodeJobAbstract = pure $ case HM.lookup "abstract" va of
          Just (Bool x) -> x
          _ -> False

        decodeJobParent :: Decoder (Maybe JobName)
        decodeJobParent = case HM.lookup "parent" va of
          Just (String p) -> pure $ Just $ JobName p
          Just Data.Aeson.Null -> pure Nothing
          Just x -> decodeFail "Invalid parent key" x
          Nothing -> pure Nothing

        decodeJobNodeset :: Decoder (Maybe JobNodeset)
        decodeJobNodeset = case HM.lookup "nodeset" va of
          Just (String n) -> pure $ Just . JobNodeset $ NodesetName n
          Just (Object nObj) -> do
            v <- decodeObjectAttribute "nodes" nObj
            case v of
              Array nodes -> Just <$> decodeJobNodesetNodes (V.toList nodes)
              -- that's weird, nodeset.nodes does not have to be a list
              o@(Object _) -> Just <$> decodeJobNodesetNodes [o]
              _ -> decodeFail "Unexpected nodes structure" (Object nObj)
          Just _va -> decodeFail "Unexpected nodeset structure" _va
          Nothing -> pure Nothing

        decodeJobDependencies :: Decoder [JobName]
        decodeJobDependencies = case HM.lookup "dependencies" va of
          Just (String v) -> pure [JobName v]
          Just (Array xs) -> traverse decodeJobDependency (V.toList xs)
          Just _ -> decodeFail "Unexpected job dependencies value" (Object va)
          Nothing -> pure []

        decodeJobDependency :: Value -> Decoder JobName
        decodeJobDependency = \case
          String v -> pure $ JobName v
          Object v -> JobName <$> getName v
          anyOther -> decodeFail "Unexpected job dependency value" anyOther

        decodeRequiredProjects :: Decoder [ProjectName]
        decodeRequiredProjects = case HM.lookup "required-projects" va of
          Just (String v) -> pure [ProjectName v]
          Just (Array xs) -> traverse decodeRequiredProject (V.toList xs)
          Just _ -> decodeFail "Unexpected required-projects value" (Object va)
          Nothing -> pure []

        decodeRequiredProject :: Value -> Decoder ProjectName
        decodeRequiredProject = \case
          String v -> pure $ ProjectName v
          Object v -> ProjectName <$> getName v
          anyOther -> decodeFail "Unexpected job required-projects value" anyOther

        decodeJobNodesetNodes xs = do
          names <- decodeNodesetNodes xs
          pure $ JobAnonymousNodeset names

        getListNames :: Text -> Object -> Decoder [Text]
        getListNames (Data.Aeson.Key.fromText -> key) obj = case HM.lookup key obj of
          Just (String v) -> pure [v]
          Just v -> traverse stringOrName =<< decodeList v
          Nothing -> pure []
          where
            stringOrName = \case
              String v -> pure v
              v -> decodeString =<< decodeObjectAttribute "name" =<< decodeObject v

        decodeSecrets = fmap SecretName <$> getListNames "secrets" va

        decodeSemaphores = fmap SemaphoreName . concat <$> sequence [deprecatedSemaphore, getListNames "semaphores" va]
          where
            deprecatedSemaphore = case HM.lookup "semaphore" va of
              Just (String v) -> pure [v]
              Just v -> do
                x <- decodeString =<< decodeObjectAttribute "name" =<< decodeObject v
                pure [x]
              Nothing -> pure []
    decodeNodesetNodes :: [Value] -> Decoder [NodeLabelName]
    decodeNodesetNodes xs = do
      names <- traverse (decodeString <=< decodeObjectAttribute "label" <=< decodeObject) xs
      pure $ NodeLabelName <$> names

    decodeNodeset :: Object -> Decoder Nodeset
    decodeNodeset va = do
      name <- NodesetName <$> getName va
      labels <- decodeNodesetNodes =<< decodeList =<< decodeObjectAttribute "nodes" va
      pure $ Nodeset {name, labels}

    decodeProject :: Object -> Decoder (BaseProject ProjectName)
    decodeProject va = do
      name <- case HM.lookup "name" va of
        (Just x) -> ProjectName <$> decodeString x
        Nothing -> pure $ ProjectName (providerName <> "/" <> projectName)
      queue <- decodeQueueName va
      templates <- decodeAsList "templates" ProjectTemplateName va
      pipelines <- Set.fromList <$> sequence (mapMaybe decodeProjectPipeline (HM.toList va))
      pure $ Project {name, queue, templates, pipelines}

    decodeQueueName :: Object -> Decoder (Maybe QueueName)
    decodeQueueName va = case HM.lookup "queue" va of
      (Just x) -> Just . QueueName <$> decodeString x
      _ -> pure Nothing

    decodeProjectPipeline :: (Data.Aeson.Key.Key, Value) -> Maybe (Decoder (ProjectPipeline ProjectName))
    decodeProjectPipeline (Data.Aeson.Key.toText -> pipelineName', va')
      -- These project configuration attribute are not pipelines
      | pipelineName' `elem` ["name", "vars", "description", "default-branch", "merge-mode", "squash-merge"] = Nothing
      | pipelineName' == "templates" = Nothing -- TODO: decode templates
      | pipelineName' == "queue" = Nothing -- TODO: decode project queues
      | otherwise = Just $ case va' of
          Object inner -> do
            let name = PipelineName pipelineName'
            jobs <- case HM.lookup "jobs" inner of
              Just (Array jobElems) -> traverse decodeProjectPipelineJob (V.toList jobElems)
              _ -> pure [] -- pipeline has no jobs
            pure $ ProjectPipeline {name, jobs}
          _ -> decodeFail "Unexpected pipeline" va'
      where
        decodeProjectPipelineJob :: Value -> Decoder (PipelineJob ProjectName)
        decodeProjectPipelineJob = \case
          String v -> pure $ PJName (JobName v)
          Object jobObj -> do
            (name, obj) <- getObjectKey jobObj
            PJJob <$> decodeJobContent (JobName name) obj
          v -> decodeFail "Unexpected project pipeline jobs format" v

    decodeProjectTemplate :: Object -> Decoder (BaseProjectTemplate ProjectName)
    decodeProjectTemplate va = do
      name <- ProjectTemplateName <$> getName va
      queue <- decodeQueueName va
      pipelines <- Set.fromList <$> sequence (mapMaybe decodeProjectPipeline (HM.toList va))
      pure $ ProjectTemplate {name, queue, pipelines}

    -- Zuul config elements are object with an unique key
    getObjectKey :: Object -> Decoder (Text, Object)
    getObjectKey hm = case HM.toList hm of
      [(Data.Aeson.Key.toText -> keyName, keyValue)] -> do
        obj <- decodeObject keyValue
        pure (keyName, obj)
      _ -> decodeFail "Top level zuul attribute is not a single key object" (Object hm)

    getName :: Object -> Decoder Text
    getName = decodeString <=< decodeObjectAttribute "name"

-- | Convenient alias
type ConnectionUrlMap = Map ProviderName ConnectionUrl

-- | The main function to decode a 'ZKFile'
loadConfig ::
  -- | The map of the connection urls.
  ConnectionUrlMap ->
  -- | The helper to resolve the matching tenant list.
  TenantResolver ->
  -- | The configuration object file to load.
  Either ConfigError ZKFile ->
  -- | The computation to update the config.
  StateT Config IO ()
loadConfig urlBuilder tenantResolver zkcE = do
  case zkcE of
    Left e -> #configErrors %= (e :)
    Right zkc ->
      let decodedResults = decodeConfig (canonicalProjectName, branchName) zkc.zkJSONData
          providerName = ProviderName $ zkc.provider
          canonicalProjectName =
            CanonicalProjectName
              providerName
              (ProjectName zkc.project)
          branchName = BranchName zkc.branch
          configPath = zkc.filePath
          -- tenants info are set in the updateTopConfig function.
          -- this is done per element because a tenant may not include everything.
          tenants = mempty :: Set Void
          url = fromMaybe (error "Missing connection provider?!") $ Map.lookup providerName urlBuilder
          configLoc = BaseConfigLoc canonicalProjectName branchName configPath url tenants
       in traverse_ (updateTopConfig tenantResolver configLoc) decodedResults

-- | An empty config.
emptyConfig :: Set TenantName -> Config
emptyConfig = Config mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty
