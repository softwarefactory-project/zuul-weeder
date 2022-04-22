{-# LANGUAGE FlexibleContexts #-}

-- |
module Zuul.ConfigLoader where

import Control.Lens (Lens', lens, over, view)
import Control.Monad.State
import Data.Aeson (Object, Value (Array, Object, String))
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM (keys, lookup, toList)
import Data.List (sort)
import Data.Map (Map, insert, lookup)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Zuul.ZKDump (ConfigError (..), ZKConfig (..))

newtype BranchName = BranchName Text deriving (Eq, Ord, Show)

newtype JobName = JobName Text deriving (Eq, Ord, Show)

newtype PipelineName = PipelineName Text deriving (Eq, Ord, Show)

newtype ProjectName = ProjectName Text deriving (Eq, Ord, Show)

newtype ProjectNameRE = ProjectNameRE Text deriving (Eq, Ord, Show)

newtype NodesetName = NodesetName Text deriving (Eq, Ord, Show)

newtype NodeLabelName = NodeLabelName Text deriving (Eq, Ord, Show)

newtype ProviderName = ProviderName Text deriving (Eq, Ord, Show)

newtype TemplateName = TemplateName Text deriving (Eq, Ord, Show)

newtype CanonicalProjectName = CanonicalProjectName (ProviderName, ProjectName) deriving (Eq, Ord, Show)

newtype ConnectionName = ConnectionName Text deriving (Eq, Ord, Show)

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
    pipelinePipelines :: [PPipeline]
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
  deriving (Show, Eq, Ord)

data ProjectConfig = ProjectConfig
  { jobs :: Map JobName Job,
    nodesets :: Map NodesetName Nodeset,
    projectPipelines :: Map Project ProjectPipeline,
    projectTemplates :: Map Project ProjectPipeline,
    pipelines :: Map PipelineName Pipeline
  }
  deriving (Show)

projectConfigNodesetsL :: Lens' ProjectConfig (Map NodesetName Nodeset)
projectConfigNodesetsL = lens nodesets (\c nodesets -> c {nodesets = nodesets})

projectConfigJobsL :: Lens' ProjectConfig (Map JobName Job)
projectConfigJobsL = lens jobs (\c jobs -> c {jobs = jobs})

projectConfigProjectPipelinesL :: Lens' ProjectConfig (Map Project ProjectPipeline)
projectConfigProjectPipelinesL = lens projectPipelines (\c pipeline -> c {projectPipelines = pipeline})

projectConfigProjectTemplatesL :: Lens' ProjectConfig (Map Project ProjectPipeline)
projectConfigProjectTemplatesL = lens projectTemplates (\c template -> c {projectTemplates = template})

projectConfigPipelinesL :: Lens' ProjectConfig (Map PipelineName Pipeline)
projectConfigPipelinesL = lens pipelines (\c pipeline -> c {pipelines = pipeline})

data Config = Config
  { configs :: Map (CanonicalProjectName, BranchName) ProjectConfig,
    configErrors :: [ConfigError]
  }
  deriving (Show)

configConfigsL :: Lens' Config (Map (CanonicalProjectName, BranchName) ProjectConfig)
configConfigsL = lens configs (\c nc -> c {configs = nc})

configErrorsL :: Lens' Config [ConfigError]
configErrorsL = lens configErrors (\c ce -> c {configErrors = ce})

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
            Object triggers -> PipelineTrigger . ConnectionName <$> HM.keys triggers
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
          pipelinePipelines = catMaybes $ decodePPipeline <$> sort (HM.toList va)
       in ProjectPipeline {..}
      where
        decodePPipeline :: (Text, Value) -> Maybe PPipeline
        decodePPipeline (pipelineName', va') = case va' of
          Object inner ->
            let pPipelineName = PipelineName pipelineName'
                pPipelineJobs = case HM.lookup "jobs" inner of
                  Just (String name) -> [PJName $ JobName name]
                  Just (Array jobElems) -> decodePPipelineJob <$> sort (V.toList jobElems)
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

    unwrapObject :: Value -> Object
    unwrapObject va = case va of
      Object hm -> hm
      _ -> error $ "Expecting an Object out of JSON Value: " <> show va
    getObjValue :: Text -> Object -> Value
    getObjValue k hm = case HM.lookup k hm of
      Just va -> va
      Nothing -> error $ "Unable to get " <> T.unpack k <> " from Object"
    -- Zuul config elements are object with an unique key
    getKey :: Object -> Text
    getKey hm = case take 1 $ HM.keys hm of
      (keyName : _) -> keyName
      [] -> error $ "Unable to get Object key on: " <> show hm
    getString :: Value -> Text
    getString va = case va of
      String str -> str
      _ -> error $ "Expected a String out of JSON value: " <> show va
    decodeAsList :: Text -> (Text -> a) -> Object -> [a]
    decodeAsList k build va = case HM.lookup k va of
      Just (String x) -> [build x]
      Just (Array xs) -> build . getString <$> sort (V.toList xs)
      Just _va -> error $ "Unexpected " <> T.unpack k <> " structure: " <> show _va
      Nothing -> []
    getName :: Object -> Text
    getName = getString . getObjValue "name"

loadConfig :: Either ConfigError ZKConfig -> StateT Config IO ()
loadConfig zkcE = do
  liftIO $ putStrLn $ "Loading " <> show zkcE
  case zkcE of
    Left e -> modify (addError e)
    Right zkc ->
      let zkcDecoded = decodeConfig projectC $ zkJSONData zkc
          projectC =
            ( CanonicalProjectName (ProviderName $ provider zkc, ProjectName $ project zkc),
              BranchName $ branch zkc
            )
       in modify (updateConfig projectC zkcDecoded)
  where
    addError :: ConfigError -> Config -> Config
    addError ce = configErrorsL `over` (ce :)
    updateConfig :: (CanonicalProjectName, BranchName) -> [ZuulConfigElement] -> Config -> Config
    updateConfig k zces config =
      let projectConfig = fromMaybe emptyProjectConfig $ Data.Map.lookup k $ view configConfigsL config
          newProjectConfig = updateProjectConfig zces projectConfig
       in over configConfigsL (insert k newProjectConfig) config

    updateProjectConfig :: [ZuulConfigElement] -> ProjectConfig -> ProjectConfig
    updateProjectConfig zces projectConfig = case zces of
      [] -> projectConfig
      (zce : xs) -> case zce of
        ZJob job ->
          updateProjectConfig xs $
            projectConfig & over projectConfigJobsL (insert (jobName job) job)
        ZNodeset node ->
          updateProjectConfig xs $
            projectConfig & over projectConfigNodesetsL (insert (nodesetName node) node)
        ZProjectPipeline project ->
          updateProjectConfig xs $
            projectConfig & over projectConfigProjectPipelinesL (insert (pName project) project)
        ZProjectTemplate template ->
          updateProjectConfig xs $
            projectConfig & over projectConfigProjectTemplatesL (insert (pName template) template)
        ZPipeline pipeline ->
          updateProjectConfig xs $
            projectConfig & over projectConfigPipelinesL (insert (pipelineName pipeline) pipeline)

-- _ -> updateProjectConfig xs projectConfig

emptyConfig :: Config
emptyConfig = Config mempty mempty

emptyProjectConfig :: ProjectConfig
emptyProjectConfig = ProjectConfig mempty mempty mempty mempty mempty
