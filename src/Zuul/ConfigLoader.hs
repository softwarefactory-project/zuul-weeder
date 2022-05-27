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
    emptyConfig,
    TenantResolver,
    ConnectionUrlMap,

    -- * Test helper
    decodeConfig,
  )
where

import Data.Aeson (Object, Value (Array, Null, String))
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified as HM (keys, lookup, toList)
import Data.Map qualified as Map
import Data.Set qualified
import Data.Vector qualified as V
import Zuul.Config
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
    projects :: ConfigMap ProjectName Project,
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
    -- | Configuration errors.
    configErrors :: [ConfigError]
  }
  deriving (Show, Generic)

updateTopConfig :: TenantResolver -> ConfigLoc -> Decoder ZuulConfigElement -> StateT Config IO ()
updateTopConfig tenantResolver configLoc (Decoder (Right ze)) = case ze of
  ZJob job -> #jobs %= insertConfig job.name job
  ZNodeset node -> do
    #nodesets %= insertConfig node.name node
    traverse_ (\v -> #nodeLabels %= insertConfig v v) $ Data.Set.fromList node.labels
  ZProject project -> #projects %= insertConfig project.name project
  ZProjectTemplate template -> #projectTemplates %= insertConfig template.name template
  ZPipeline pipeline -> do
    #pipelines %= insertConfig pipeline.name pipeline
    traverse_ (\(PipelineTrigger v) -> #triggers %= insertConfig v v) pipeline.triggers
  ZSecret secret -> #secrets %= insertConfig secret secret
  ZQueue queue -> #queues %= insertConfig queue queue
  ZSemaphore semaphore -> #semaphores %= insertConfig semaphore semaphore
  where
    tenants = tenantResolver configLoc (from ze)
    insertConfig k v
      | null tenants = id -- The object is not attached to any tenant, we don't add it
      | otherwise = Map.insertWith mappend k [(configLoc {tenants = tenants}, v)]
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
      triggers <- case decodeObject =<< decodeObjectAttribute "trigger" va of
        Decoder (Left _) -> pure mempty
        Decoder (Right triggersObj) ->
          pure $
            PipelineTrigger . ConnectionName . Data.Aeson.Key.toText <$> HM.keys triggersObj
      pure $ Pipeline {name, triggers}

    decodeJob :: Object -> Decoder Job
    decodeJob va = do
      name <- JobName <$> getName va
      decodeJobContent name va

    decodeJobContent :: JobName -> Object -> Decoder Job
    decodeJobContent name va = do
      Job name
        <$> decodeJobParent
        <*> decodeJobNodeset
        <*> decodeAsList "branches" BranchName va
        <*> decodeJobDependencies
      where
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

        decodeJobNodesetNodes xs = do
          names <- decodeNodesetNodes xs
          pure $ JobAnonymousNodeset names

    decodeNodesetNodes :: [Value] -> Decoder [NodeLabelName]
    decodeNodesetNodes xs = do
      names <- traverse (decodeString <=< decodeObjectAttribute "label" <=< decodeObject) xs
      pure $ NodeLabelName <$> names

    decodeNodeset :: Object -> Decoder Nodeset
    decodeNodeset va = do
      name <- NodesetName <$> getName va
      labels <- decodeNodesetNodes =<< decodeList =<< decodeObjectAttribute "nodes" va
      pure $ Nodeset {name, labels}

    decodeProject :: Object -> Decoder Project
    decodeProject va = do
      name <- case HM.lookup "name" va of
        (Just x) -> ProjectName <$> decodeString x
        Nothing -> pure $ ProjectName (providerName <> "/" <> projectName)
      templates <- decodeAsList "templates" ProjectTemplateName va
      pipelines <- Data.Set.fromList <$> sequence (mapMaybe decodeProjectPipeline (HM.toList va))
      pure $ Project {name, templates, pipelines}

    decodeProjectPipeline :: (Data.Aeson.Key.Key, Value) -> Maybe (Decoder ProjectPipeline)
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
        decodeProjectPipelineJob :: Value -> Decoder PipelineJob
        decodeProjectPipelineJob = \case
          String v -> pure $ PJName (JobName v)
          Object jobObj -> do
            (name, obj) <- getObjectKey jobObj
            PJJob <$> decodeJobContent (JobName name) obj
          v -> decodeFail "Unexpected project pipeline jobs format" v

    decodeProjectTemplate :: Object -> Decoder ProjectTemplate
    decodeProjectTemplate va = do
      name <- ProjectTemplateName <$> getName va
      pipelines <- Data.Set.fromList <$> sequence (mapMaybe decodeProjectPipeline (HM.toList va))
      pure $ ProjectTemplate {name, pipelines}

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

-- | Convenient alias
type TenantResolver = ConfigLoc -> ZuulConfigType -> Set TenantName

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
          tenants = mempty
          url = fromMaybe (error "Missing connection provider?!") $ Map.lookup providerName urlBuilder
          configLoc = ConfigLoc canonicalProjectName branchName configPath url tenants
       in traverse_ (updateTopConfig tenantResolver configLoc) decodedResults

-- | An empty config.
emptyConfig :: Config
emptyConfig = Config mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty
