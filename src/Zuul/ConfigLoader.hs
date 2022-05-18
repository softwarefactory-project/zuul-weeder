module Zuul.ConfigLoader
  ( -- * The resulting configuration
    Config (..),
    ConfigMap,
    loadConfig,
    emptyConfig,
    TenantResolver,
    UrlBuilder,

    -- * Test helper
    decodeConfig,
  )
where

import Data.Aeson (Object, Value (Array, Object, String))
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified as HM (keys, lookup, toList)
import Data.Map qualified as Map
import Data.Set qualified
import Data.Vector qualified as V
import Zuul.Config
import Zuul.ZooKeeper (ConfigError (..), ZKConfig (..))
import ZuulWeeder.Prelude

type ConfigMap a b = Map a [(ConfigLoc, b)]

data Config = Config
  { jobs :: ConfigMap JobName Job,
    nodesets :: ConfigMap NodesetName Nodeset,
    nodeLabels :: ConfigMap NodeLabelName NodeLabelName,
    projects :: ConfigMap ProjectName Project,
    projectTemplates :: ConfigMap ProjectTemplateName ProjectTemplate,
    pipelines :: ConfigMap PipelineName Pipeline,
    configErrors :: [ConfigError]
  }
  deriving (Show, Generic)

updateTopConfig :: TenantResolver -> ConfigLoc -> ZuulConfigElement -> StateT Config IO ()
updateTopConfig tenantResolver configLoc ze = case ze of
  ZJob job -> #jobs %= insertConfig job.name job
  ZNodeset node -> do
    #nodesets %= insertConfig node.name node
    traverse_ (\v -> #nodeLabels %= insertConfig v v) $ Data.Set.fromList node.labels
  ZProject project ->
    -- TODO: add PJJob to the list of jobs?
    #projects %= insertConfig project.name project
  ZProjectTemplate template -> #projectTemplates %= insertConfig template.name template
  ZPipeline pipeline -> #pipelines %= insertConfig pipeline.name pipeline
  _ -> error "Not implemented"
  where
    tenants = tenantResolver configLoc (from ze)
    insertConfig k v
      | null tenants = id -- The object is not attached to any tenant, we don't add it
      | otherwise = Map.insertWith mappend k [(configLoc {tenants = tenants}, v)]

decodeConfig :: (CanonicalProjectName, BranchName) -> Value -> [ZuulConfigElement]
decodeConfig (CanonicalProjectName (ProviderName providerName, ProjectName projectName), _branch) zkJSONData =
  let rootObjs = case zkJSONData of
        Array vec -> V.toList vec
        _ -> error $ "Unexpected root data structure on: " <> show rootObjs
   in mapMaybe getConfigElement rootObjs
  where
    getConfigElement :: Value -> Maybe ZuulConfigElement
    getConfigElement rootObj =
      let obj = unwrapObject rootObj
       in case getKey obj of
            "job" -> Just . ZJob . decodeJob $ getE "job" obj
            "nodeset" -> Just . ZNodeset . decodeNodeset $ getE "nodeset" obj
            "project" -> Just . ZProject . decodeProject $ getE "project" obj
            -- We use the same decoder as for "project" as the structure is slightly the same
            "project-template" -> Just . ZProjectTemplate . decodeProjectTemplate $ getE "project-template" obj
            "pipeline" -> Just . ZPipeline . decodePipeline $ getE "pipeline" obj
            _ -> Nothing
      where
        getE :: Text -> Object -> Object
        getE k o = unwrapObject $ getObjValue k o

    decodePipeline :: Object -> Pipeline
    decodePipeline va =
      let name = PipelineName $ getName va
          triggers = case getObjValue "trigger" va of
            Object triggers' -> PipelineTrigger . ConnectionName . Data.Aeson.Key.toText <$> HM.keys triggers'
            _ -> error $ "Unexpected trigger value in: " <> show va
       in Pipeline {..}

    decodeJob :: Object -> Job
    decodeJob va =
      let name = JobName $ getName va
          ( parent,
            nodeset,
            branches,
            dependencies
            ) = decodeJobContent va
       in Job {..}

    decodeJobContent :: Object -> (Maybe JobName, Maybe JobNodeset, [BranchName], [JobName])
    decodeJobContent va =
      let jobParent = case HM.lookup "parent" va of
            Just (String p) -> Just $ JobName p
            _ -> Nothing
          jobNodeset = decodeJobNodeset
          jobBranches = decodeJobBranches
          jobDependencies = decodeJobDependencies
       in (jobParent, jobNodeset, jobBranches, jobDependencies)
      where
        decodeJobNodeset :: Maybe JobNodeset
        decodeJobNodeset = case HM.lookup "nodeset" va of
          Just (String n) -> Just . JobNodeset $ NodesetName n
          Just (Object nObj) -> case getObjValue "nodes" nObj of
            Array nodes ->
              Just $
                JobAnonymousNodeset $
                  NodeLabelName . getString . getObjValue "label"
                    <$> (unwrapObject <$> sort (V.toList nodes))
            _ -> error $ "Unexpected nodeset nodes structure: " <> show nObj
          Just _va -> error $ "Unexpected nodeset structure: " <> show _va
          Nothing -> Nothing
        decodeJobBranches :: [BranchName]
        decodeJobBranches = decodeAsList "branches" BranchName va
        decodeJobDependencies :: [JobName]
        decodeJobDependencies = case HM.lookup "dependencies" va of
          Just (String v) -> [JobName v]
          Just (Array xs) -> map decodeJobDependency (V.toList xs)
          Just _ -> error $ "Unexpected job dependencies value: " <> show va
          Nothing -> []
        decodeJobDependency :: Value -> JobName
        decodeJobDependency = \case
          String v -> JobName v
          Object (HM.lookup "name" -> Just (String v)) -> JobName v
          anyOther -> error $ "Unexpected job dependency value: " <> show anyOther

    decodeNodeset :: Object -> Nodeset
    decodeNodeset va =
      let name = NodesetName . getString $ getObjValue "name" va
          labels = case getObjValue "nodes" va of
            Array nodes ->
              NodeLabelName . getString . getObjValue "label"
                <$> (unwrapObject <$> sort (V.toList nodes))
            _va -> error $ "Unexpected nodeset nodes structure: " <> show _va
       in Nodeset {..}

    decodeProject :: Object -> Project
    decodeProject va =
      let name = case getString <$> HM.lookup "name" va of
            -- project name default to the name of the project where the config is found
            Nothing -> ProjectName (providerName <> "/" <> projectName)
            Just name' -> ProjectName name'
          templates = decodeAsList "templates" ProjectTemplateName va
          pipelines = Data.Set.fromList $ mapMaybe decodeProjectPipeline (HM.toList va)
       in Project {..}

    decodeProjectPipeline :: (Data.Aeson.Key.Key, Value) -> Maybe ProjectPipeline
    decodeProjectPipeline (Data.Aeson.Key.toText -> pipelineName', va')
      | pipelineName' `elem` ["name", "vars", "description"] = Nothing
      | pipelineName' == "templates" = Nothing -- TODO: decode templates
      | pipelineName' == "queue" = Nothing -- TODO: decode project queues
      | otherwise = case va' of
          Object inner ->
            let name = PipelineName pipelineName'
                jobs = case HM.lookup "jobs" inner of
                  -- Just (String name') -> [PJName $ JobName name']
                  Just (Array jobElems) -> decodeProjectPipelineJob <$> V.toList jobElems
                  _ -> [] -- pipeline has no jobs
                  -- TODO: decode pipeline queue
                _queue = case HM.lookup "queue" inner of
                  Just (String queueName) -> Just queueName
                  Just _ -> error $ "Unexpected queue name: " <> show inner
                  Nothing -> Nothing
             in Just $ ProjectPipeline {..}
          _ -> error $ "Unexpected pipeline: " <> show va'
      where
        decodeProjectPipelineJob :: Value -> PipelineJob
        decodeProjectPipelineJob jobElem = case jobElem of
          Object jobObj ->
            let key = getKey jobObj
                name = JobName key
                ( parent,
                  nodeset,
                  branches,
                  dependencies
                  ) = decodeJobContent $ unwrapObject $ getObjValue key jobObj
             in PJJob $ Job {..}
          String v -> PJName $ JobName v
          _ -> error $ "Unexpected project pipeline jobs format: " <> show jobElem

    decodeProjectTemplate :: Object -> ProjectTemplate
    decodeProjectTemplate va =
      case getString <$> HM.lookup "name" va of
        Nothing -> error "Un-named project template"
        Just (ProjectTemplateName -> name) ->
          let pipelines = Data.Set.fromList $ mapMaybe decodeProjectPipeline (HM.toList va)
           in ProjectTemplate {..}

    -- Zuul config elements are object with an unique key
    getKey :: Object -> Text
    getKey hm = case take 1 $ HM.keys hm of
      (keyName : _) -> Data.Aeson.Key.toText keyName
      [] -> error $ "Unable to get Object key on: " <> show hm

    getName :: Object -> Text
    getName = getString . getObjValue "name"

type UrlBuilder = Map ProviderName ConnectionUrl

type TenantResolver = ConfigLoc -> ZuulConfigType -> Set TenantName

loadConfig :: UrlBuilder -> TenantResolver -> Either ConfigError ZKConfig -> StateT Config IO ()
loadConfig urlBuilder tenantResolver zkcE = do
  case zkcE of
    Left e -> #configErrors %= (e :)
    Right zkc -> flip catchAll (handler zkc.fullPath) do
      let zkcDecoded = decodeConfig (canonicalProjectName, branchName) $ zkJSONData zkc
          providerName = ProviderName $ zkc.provider
          canonicalProjectName =
            CanonicalProjectName
              ( providerName,
                ProjectName $ zkc.project
              )
          branchName = BranchName zkc.branch
          configPath = zkc.filePath
          -- tenants info are set in the updateTopConfig function.
          -- this is done per element because a tenant may not include everything.
          tenants = mempty
          url = fromMaybe (error "Missing connection provider?!") $ Map.lookup providerName urlBuilder
          configLoc = ConfigLoc canonicalProjectName branchName configPath url tenants
       in traverse_ (updateTopConfig tenantResolver configLoc) zkcDecoded
  where
    handler :: FilePathT -> SomeException -> StateT Config IO ()
    handler fp e = do
      lift $ hPutStrLn stderr $ "Could not decode: " <> getPath' fp <> ": " <> show e
      error "Aborting"

emptyConfig :: Config
emptyConfig = Config mempty mempty mempty mempty mempty mempty mempty
