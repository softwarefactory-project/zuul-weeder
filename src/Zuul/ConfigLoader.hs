module Zuul.ConfigLoader where

import Control.Lens ((%=))
import Control.Monad.State
import Data.Aeson (Object, Value (Array, Object, String))
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified as HM (keys, lookup, toList)
import Data.Foldable (traverse_)
import Data.Generics.Labels ()
import Data.List (sort)
import Data.Map (Map, insertWith)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Display qualified
import Data.Text.Lazy.Builder qualified as TB
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Witch qualified
import Zuul.ZKDump (ConfigError (..), ZKConfig (..))

newtype BranchName = BranchName Text deriving (Eq, Ord, Show)

newtype JobName = JobName Text
  deriving (Eq, Ord, Show)
  deriving (Data.Text.Display.Display) via (Data.Text.Display.ShowInstance JobName)

newtype PipelineName = PipelineName Text deriving (Eq, Ord, Show)

newtype ProjectName = ProjectName Text deriving (Eq, Ord, Show)

newtype ProjectNameRE = ProjectNameRE Text deriving (Eq, Ord, Show)

newtype NodesetName = NodesetName Text
  deriving (Eq, Ord, Show)
  deriving (Data.Text.Display.Display) via (Data.Text.Display.ShowInstance NodesetName)

newtype NodeLabelName = NodeLabelName Text
  deriving (Eq, Ord, Show)
  deriving (Data.Text.Display.Display) via (Data.Text.Display.ShowInstance NodeLabelName)

newtype ProviderName = ProviderName Text deriving (Eq, Ord, Show)

newtype TemplateName = TemplateName Text deriving (Eq, Ord, Show)

newtype CanonicalProjectName = CanonicalProjectName (ProviderName, ProjectName) deriving (Eq, Ord, Show)

newtype ConnectionName = ConnectionName Text deriving (Eq, Ord, Show)

newtype TenantName = TenantName Data.Text.Text deriving (Show, Eq, Ord)

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
  { nodesetName :: NodesetName,
    nodesetLabels :: [NodeLabelName]
  }
  deriving (Show, Eq, Ord)

data Job = Job
  { jobName :: JobName,
    jobParent :: Maybe JobName,
    jobNodeset :: Maybe JobNodeset,
    jobBranches :: [BranchName],
    jobDependencies :: [JobName]
  }
  deriving (Show, Eq, Ord)

data PipelineJob
  = PJName JobName
  | PJJob Job
  deriving (Show, Eq, Ord)

data PPipeline = PPipeline
  { pPipelineName :: PipelineName,
    pPipelineJobs :: [PipelineJob]
  }
  deriving (Show, Eq, Ord)

data ProjectPipeline = ProjectPipeline
  { pName :: Project,
    pipelineTemplates :: [TemplateName],
    pipelinePipelines :: Data.Set.Set PPipeline
  }
  deriving (Show, Eq, Ord)

newtype PipelineTrigger = PipelineTrigger {connectionName :: ConnectionName}
  deriving (Show, Ord, Eq)

data Pipeline = Pipeline
  { pipelineName :: PipelineName,
    pipelineTriggers :: [PipelineTrigger]
  }
  deriving (Show, Eq, Ord)

data ZuulConfigElement
  = ZJob Job
  | ZProjectPipeline ProjectPipeline
  | ZNodeset Nodeset
  | ZProjectTemplate ProjectPipeline
  | ZPipeline Pipeline
  | ZNodeLabel NodeLabelName
  deriving (Show, Eq, Ord)

data ZuulConfigType
  = PipelineT
  | JobT
  | SemaphoreT
  | ProjectT
  | ProjectTemplateT
  | NodesetT
  | SecretT
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Witch.From ZuulConfigElement ZuulConfigType where
  from zce = case zce of
    ZJob _ -> JobT
    ZProjectPipeline _ -> ProjectT
    ZNodeset _ -> NodesetT
    ZProjectTemplate _ -> ProjectTemplateT
    ZPipeline _ -> PipelineT
    -- ZNodeLabel is only used for cli input, it is not a real ZCE
    ZNodeLabel _ -> error "The impossible has happened"

instance Data.Text.Display.Display ZuulConfigElement where
  displayBuilder zce = case zce of
    ZJob job -> Data.Text.Display.displayBuilder $ jobName job
    ZNodeset ns -> Data.Text.Display.displayBuilder $ nodesetName ns
    ZNodeLabel label -> Data.Text.Display.displayBuilder label
    _ -> TB.fromText "unknown"

newtype ConfigPath = ConfigPath {getConfigPath :: FilePath} deriving (Show, Eq, Ord)

instance Data.Text.Display.Display ConfigPath where
  displayBuilder (ConfigPath p) = TB.fromText (T.pack p)

data ConfigLoc = ConfigLoc
  { clProject :: CanonicalProjectName,
    clBranch :: BranchName,
    clPath :: ConfigPath,
    clTenants :: [TenantName]
  }
  deriving (Show, Eq, Ord)

instance Data.Text.Display.Display CanonicalProjectName where
  displayBuilder (CanonicalProjectName (_, ProjectName projectName)) = TB.fromText projectName

instance Data.Text.Display.Display ConfigLoc where
  displayBuilder ConfigLoc {..} =
    Data.Text.Display.displayBuilder clProject
      <> branchBuilder
      <> TB.fromText ": "
      <> Data.Text.Display.displayBuilder clPath
    where
      branchBuilder = case clBranch of
        BranchName "master" -> ""
        BranchName n -> TB.fromText $ "[" <> n <> "]"

type ConfigMap a b = Map a [(ConfigLoc, b)]

data Config = Config
  { configJobs :: ConfigMap JobName Job,
    configNodesets :: ConfigMap NodesetName Nodeset,
    configNodelabels :: ConfigMap NodeLabelName NodeLabelName,
    configProjectPipelines :: ConfigMap Project ProjectPipeline,
    configProjectTemplates :: ConfigMap Project ProjectPipeline,
    configPipelines :: ConfigMap PipelineName Pipeline,
    configErrors :: [ConfigError]
  }
  deriving (Show, Generic)

updateTopConfig :: TenantResolver -> ConfigLoc -> ZuulConfigElement -> StateT Config IO ()
updateTopConfig tenantResolver configLoc ze = case ze of
  ZJob job -> #configJobs %= insertConfig (jobName job) job
  ZNodeset node -> do
    #configNodesets %= insertConfig (nodesetName node) node
    traverse_ (\v -> #configNodelabels %= insertConfig v v) $ Data.Set.fromList (nodesetLabels node)
  ZProjectPipeline project -> #configProjectPipelines %= insertConfig (pName project) project
  ZProjectTemplate template -> #configProjectTemplates %= insertConfig (pName template) template
  ZPipeline pipeline -> #configPipelines %= insertConfig (pipelineName pipeline) pipeline
  ZNodeLabel _ -> pure ()
  where
    tenants = tenantResolver configLoc (Witch.from ze)
    insertConfig k v = Data.Map.insertWith mappend k [(configLoc {clTenants = tenants}, v)]

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
      let pipelineName = PipelineName $ getName va
          pipelineTriggers = case getObjValue "trigger" va of
            Object triggers -> PipelineTrigger . ConnectionName . Data.Aeson.Key.toText <$> HM.keys triggers
            _ -> error $ "Unexpected trigger value in: " <> show va
       in Pipeline {..}
    decodeJob :: Object -> Job
    decodeJob va =
      let jobName = JobName $ getName va
          ( jobParent,
            jobNodeset,
            jobBranches,
            jobDependencies
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
      let nodesetName = NodesetName . getString $ getObjValue "name" va
          nodesetLabels = case getObjValue "nodes" va of
            Array nodes ->
              NodeLabelName . getString . getObjValue "label"
                <$> (unwrapObject <$> sort (V.toList nodes))
            _va -> error $ "Unexpected nodeset nodes structure: " <> show _va
       in Nodeset {..}

    decodeProjectPipeline :: Object -> ProjectPipeline
    decodeProjectPipeline va =
      let pName = case getString <$> HM.lookup "name" va of
            Nothing -> PNameCannonical project
            Just name -> case T.uncons name of
              Just ('^', _) -> PNameRE $ ProjectNameRE name
              Just _ -> PName $ ProjectName name
              Nothing -> error $ "Unexpected project name for project pipeline: " <> T.unpack name
          pipelineTemplates = decodeAsList "templates" TemplateName va
          pipelinePipelines = Data.Set.fromList $ mapMaybe decodePPipeline (HM.toList va)
       in ProjectPipeline {..}
      where
        decodePPipeline :: (Data.Aeson.Key.Key, Value) -> Maybe PPipeline
        decodePPipeline (Data.Aeson.Key.toText -> pipelineName', va') = case va' of
          Object inner ->
            let pPipelineName = PipelineName pipelineName'
                pPipelineJobs = case HM.lookup "jobs" inner of
                  Just (String name) -> [PJName $ JobName name]
                  Just (Array jobElems) -> decodePPipelineJob <$> V.toList jobElems
                  _ -> error $ "Unexpected project pipeline format: " <> show inner
             in Just $ PPipeline {..}
          _ -> Nothing
        decodePPipelineJob :: Value -> PipelineJob
        decodePPipelineJob jobElem = case jobElem of
          Object jobObj ->
            let key = getKey jobObj
                jobName = JobName key
                ( jobParent,
                  jobNodeset,
                  jobBranches,
                  jobDependencies
                  ) = decodeJobContent $ unwrapObject $ getObjValue key jobObj
             in PJJob $ Job {..}
          String v -> PJName $ JobName v
          _ -> error $ "Unexpected project pipeline jobs format: " <> show jobElem
    decodeProjectTemplate :: Object -> ProjectPipeline
    decodeProjectTemplate va =
      let template = decodeProjectPipeline va
       in template {pName = TName . getPName $ pName template}
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
  Just _va -> error $ "Unexpected " <> T.unpack k <> " structure: " <> show _va
  Nothing -> []

unwrapObject :: Value -> Object
unwrapObject va = case va of
  Object hm -> hm
  _ -> error $ "Expecting an Object out of JSON Value: " <> show va

getObjValue :: Text -> Object -> Value
getObjValue k hm = case HM.lookup (Data.Aeson.Key.fromText k) hm of
  Just va -> va
  Nothing -> error $ "Unable to get " <> T.unpack k <> " from Object: " <> show (HM.keys hm)

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
              ( ProviderName $ provider zkc,
                ProjectName $ project zkc
              )
          branchName = BranchName $ branch zkc
          configPath = ConfigPath (T.unpack $ filePath zkc)
          -- tenants info are set in the updateTopConfig function.
          -- this is done per element because a tenant may not include everything.
          tenants = []
          configLoc = ConfigLoc canonicalProjectName branchName configPath tenants
       in traverse_ (updateTopConfig tenantResolver configLoc) zkcDecoded

emptyConfig :: Config
emptyConfig = Config mempty mempty mempty mempty mempty mempty mempty
