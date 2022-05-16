module Zuul.ConfigLoader where

import Data.Aeson (Object, Value (Array, Object, String))
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified as HM (keys, lookup, toList)
import Data.Map (insertWith)
import Data.Set qualified
import Data.Text qualified as Text
import Data.Text.Lazy.Builder qualified as TB
import Data.Vector qualified as V
import Zuul.ZKDump (ConfigError (..), ZKConfig (..))
import ZuulWeeder.Prelude

-- | The name of any configuration object, provided by the 'From X ConfigName` instance.
newtype ConfigName = ConfigName Text deriving (Eq, Ord, Show)

newtype BranchName = BranchName Text deriving (Eq, Ord, Show)

newtype JobName = JobName Text
  deriving (Eq, Ord, Show)
  deriving (Display) via (ShowInstance JobName)

newtype PipelineName = PipelineName Text deriving (Eq, Ord, Show)

newtype ProjectName = ProjectName Text deriving (Eq, Ord, Show)

newtype ProjectNameRE = ProjectNameRE Text deriving (Eq, Ord, Show)

newtype NodesetName = NodesetName Text
  deriving (Eq, Ord, Show)
  deriving (Display) via (ShowInstance NodesetName)

newtype NodeLabelName = NodeLabelName Text
  deriving (Eq, Ord, Show)
  deriving (Display) via (ShowInstance NodeLabelName)

newtype ProviderName = ProviderName Text deriving (Eq, Ord, Show)

newtype TemplateName = TemplateName Text deriving (Eq, Ord, Show)

newtype CanonicalProjectName = CanonicalProjectName (ProviderName, ProjectName) deriving (Eq, Ord, Show)

newtype ConnectionName = ConnectionName Text deriving (Eq, Ord, Show)

newtype TenantName = TenantName Text deriving (Show, Eq, Ord)

data Project
  = PName ProjectName
  | TName TemplateName
  | PNameRE ProjectNameRE
  | PNameCannonical CanonicalProjectName
  deriving (Eq, Ord, Show)

data JobNodeset
  = JobNodeset NodesetName
  | JobAnonymousNodeset [NodeLabelName]
  deriving (Eq, Ord, Show)

data Nodeset = Nodeset
  { name :: NodesetName,
    labels :: [NodeLabelName]
  }
  deriving (Show, Eq, Ord)

data Job = Job
  { name :: JobName,
    parent :: Maybe JobName,
    nodeset :: Maybe JobNodeset,
    branches :: [BranchName],
    dependencies :: [JobName]
  }
  deriving (Show, Eq, Ord)

instance From Job ConfigName where
  from job =
    let JobName name = job.name
     in ConfigName name

data PipelineJob
  = PJName JobName
  | PJJob Job
  deriving (Show, Eq, Ord)

data PPipeline = PPipeline
  { name :: PipelineName,
    jobs :: [PipelineJob]
  }
  deriving (Show, Eq, Ord)

data ProjectPipeline = ProjectPipeline
  { name :: Project,
    templates :: [TemplateName],
    pipelines :: Data.Set.Set PPipeline
  }
  deriving (Show, Eq, Ord, Generic)

newtype PipelineTrigger = PipelineTrigger {connectionName :: ConnectionName}
  deriving (Show, Ord, Eq)

data Pipeline = Pipeline
  { name :: PipelineName,
    triggers :: [PipelineTrigger]
  }
  deriving (Show, Eq, Ord)

data ZuulConfigElement
  = ZJob Job
  | ZProjectPipeline ProjectPipeline
  | ZNodeset Nodeset
  | ZProjectTemplate ProjectPipeline
  | ZPipeline Pipeline
  | ZQueue Queue
  | ZSemaphore Semaphore
  deriving (Show, Eq, Ord)

data ZuulConfigType
  = PipelineT
  | JobT
  | SemaphoreT
  | ProjectT
  | ProjectTemplateT
  | NodesetT
  | SecretT
  | QueueT
  deriving (Show, Eq, Ord, Enum, Bounded)

instance From ZuulConfigElement ZuulConfigType where
  from zce = case zce of
    ZJob _ -> JobT
    ZProjectPipeline _ -> ProjectT
    ZNodeset _ -> NodesetT
    ZProjectTemplate _ -> ProjectTemplateT
    ZPipeline _ -> PipelineT
    ZQueue _ -> QueueT
    ZSemaphore _ -> SemaphoreT

instance From ZuulConfigType Text where
  from zce = case zce of
    JobT -> "job"
    _ -> error "Not Implemented (yet)"

data Queue = Queue {name :: Text, perBranch :: Bool} deriving (Show, Eq, Ord)

data Semaphore = Semaphore {name :: Text, max :: Int} deriving (Show, Eq, Ord)

instance Display ZuulConfigElement where
  displayBuilder zce = case zce of
    ZJob job -> displayBuilder $ job.name
    ZNodeset ns -> displayBuilder $ ns.name
    _ -> TB.fromText "unknown"

data ConfigLoc = ConfigLoc
  { project :: CanonicalProjectName,
    branch :: BranchName,
    path :: FilePathT,
    tenants :: [TenantName]
  }
  deriving (Show, Eq, Ord, Generic)

instance Display CanonicalProjectName where
  displayBuilder (CanonicalProjectName (_, ProjectName projectName)) = TB.fromText projectName

instance Display ConfigLoc where
  displayBuilder loc =
    displayBuilder loc.project
      <> branchBuilder
      <> TB.fromText ": "
      <> displayBuilder loc.path
    where
      branchBuilder = case loc.branch of
        BranchName "master" -> ""
        BranchName n -> TB.fromText $ "[" <> n <> "]"

type ConfigMap a b = Map a [(ConfigLoc, b)]

data Config = Config
  { jobs :: ConfigMap JobName Job,
    nodesets :: ConfigMap NodesetName Nodeset,
    nodeLabels :: ConfigMap NodeLabelName NodeLabelName,
    projectPipelines :: ConfigMap Project ProjectPipeline,
    projectTemplates :: ConfigMap Project ProjectPipeline,
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
  ZProjectPipeline project ->
    -- TODO: add PJJob to the list of jobs?
    #projectPipelines %= insertConfig project.name project
  ZProjectTemplate template -> #projectTemplates %= insertConfig template.name template
  ZPipeline pipeline -> #pipelines %= insertConfig pipeline.name pipeline
  _ -> error "Not implemented"
  where
    tenants = tenantResolver configLoc (from ze)
    insertConfig k v = Data.Map.insertWith mappend k [(configLoc {tenants = tenants}, v)]

decodeConfig :: (CanonicalProjectName, BranchName) -> Value -> [ZuulConfigElement]
decodeConfig (project, _branch) zkJSONData =
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
            "project" -> Just . ZProjectPipeline . decodeProjectPipeline $ getE "project" obj
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
        decodeJobDependencies = decodeAsList "dependencies" JobName va

    decodeNodeset :: Object -> Nodeset
    decodeNodeset va =
      let name = NodesetName . getString $ getObjValue "name" va
          labels = case getObjValue "nodes" va of
            Array nodes ->
              NodeLabelName . getString . getObjValue "label"
                <$> (unwrapObject <$> sort (V.toList nodes))
            _va -> error $ "Unexpected nodeset nodes structure: " <> show _va
       in Nodeset {..}

    decodeProjectPipeline :: Object -> ProjectPipeline
    decodeProjectPipeline va =
      let name = case getString <$> HM.lookup "name" va of
            Nothing -> PNameCannonical project
            Just name' -> case Text.uncons name' of
              Just ('^', _) -> PNameRE $ ProjectNameRE name'
              Just _ -> PName $ ProjectName name'
              Nothing -> error $ "Unexpected project name for project pipeline: " <> Text.unpack name'
          templates = decodeAsList "templates" TemplateName va
          pipelines = Data.Set.fromList $ mapMaybe decodePPipeline (HM.toList va)
       in ProjectPipeline {..}
      where
        decodePPipeline :: (Data.Aeson.Key.Key, Value) -> Maybe PPipeline
        decodePPipeline (Data.Aeson.Key.toText -> pipelineName', va') = case va' of
          Object inner ->
            let name = PipelineName pipelineName'
                jobs = case HM.lookup "jobs" inner of
                  Just (String name') -> [PJName $ JobName name']
                  Just (Array jobElems) -> decodePPipelineJob <$> V.toList jobElems
                  _ -> error $ "Unexpected project pipeline format: " <> show inner
             in Just $ PPipeline {..}
          _ -> Nothing
        decodePPipelineJob :: Value -> PipelineJob
        decodePPipelineJob jobElem = case jobElem of
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
    decodeProjectTemplate :: Object -> ProjectPipeline
    decodeProjectTemplate va =
      let template = decodeProjectPipeline va
       in template & #name `set` (TName . getPName $ template.name)
      where
        getPName :: Project -> TemplateName
        getPName (PName (ProjectName name)) = TemplateName name
        getPName _va = error $ "Unexpected template name: " <> show _va

    -- Zuul config elements are object with an unique key
    getKey :: Object -> Text
    getKey hm = case take 1 $ HM.keys hm of
      (keyName : _) -> Data.Aeson.Key.toText keyName
      [] -> error $ "Unable to get Object key on: " <> show hm

    getName :: Object -> Text
    getName = getString . getObjValue "name"

decodeAsList :: Text -> (Text -> a) -> Object -> [a]
decodeAsList k build va = case HM.lookup (Data.Aeson.Key.fromText k) va of
  Just (String x) -> [build x]
  Just (Array xs) -> build . getString <$> sort (V.toList xs)
  Just _va -> error $ "Unexpected " <> Text.unpack k <> " structure: " <> show _va
  Nothing -> []

unwrapObject :: Value -> Object
unwrapObject va = case va of
  Object hm -> hm
  _ -> error $ "Expecting an Object out of JSON Value: " <> show va

getObjValue :: Text -> Object -> Value
getObjValue k hm = case HM.lookup (Data.Aeson.Key.fromText k) hm of
  Just va -> va
  Nothing -> error $ "Unable to get " <> Text.unpack k <> " from Object: " <> show (HM.keys hm)

getString :: Value -> Text
getString va = case va of
  String str -> str
  _ -> error $ "Expected a String out of JSON value: " <> show va

type TenantResolver = ConfigLoc -> ZuulConfigType -> [TenantName]

loadConfig :: TenantResolver -> Either ConfigError ZKConfig -> StateT Config IO ()
loadConfig tenantResolver zkcE = do
  case zkcE of
    Left e -> #configErrors %= (e :)
    Right zkc ->
      let zkcDecoded = decodeConfig (canonicalProjectName, branchName) $ zkJSONData zkc
          canonicalProjectName =
            CanonicalProjectName
              ( ProviderName $ zkc.provider,
                ProjectName $ zkc.project
              )
          branchName = BranchName zkc.branch
          configPath = zkc.filePath
          -- tenants info are set in the updateTopConfig function.
          -- this is done per element because a tenant may not include everything.
          tenants = []
          configLoc = ConfigLoc canonicalProjectName branchName configPath tenants
       in traverse_ (updateTopConfig tenantResolver configLoc) zkcDecoded

emptyConfig :: Config
emptyConfig = Config mempty mempty mempty mempty mempty mempty mempty
