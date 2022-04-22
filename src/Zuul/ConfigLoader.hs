{-# LANGUAGE FlexibleContexts #-}

-- |
module Zuul.ConfigLoader where

import Control.Lens (Lens', lens, over, view)
import Control.Monad.State
import Data.Aeson (Object, Value (Array, Object, String))
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

data Project
  = PName ProjectName
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

data Pipeline = Pipeline
  { pipelineName :: PipelineName,
    pipelineJobs :: [PipelineJob]
  }
  deriving (Show, Eq, Ord)

data ProjectPipeline = ProjectPipeline
  { pName :: Project,
    pipelineTemplates :: [TemplateName],
    pipelinePipelines :: [Pipeline]
  }
  deriving (Show, Eq, Ord)

data ZuulConfigElement
  = ZJob Job
  | ZProjectPipeline ProjectPipeline
  | ZNodeset Nodeset
  | ZProjectTemplate ProjectPipeline
  deriving (Show, Eq, Ord)

data ProjectConfig = ProjectConfig
  { jobs :: Map JobName Job,
    nodesets :: Map NodesetName Nodeset,
    projectPipelines :: Map Project ProjectPipeline
  }
  deriving (Show)

projectConfigNodesetsL :: Lens' ProjectConfig (Map NodesetName Nodeset)
projectConfigNodesetsL = lens nodesets (\c nodesets -> c {nodesets = nodesets})

projectConfigJobsL :: Lens' ProjectConfig (Map JobName Job)
projectConfigJobsL = lens jobs (\c jobs -> c {jobs = jobs})

projectConfigProjectsL :: Lens' ProjectConfig (Map Project ProjectPipeline)
projectConfigProjectsL = lens projectPipelines (\c project -> c {projectPipelines = project})

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
            "job" -> Just . ZJob . decodeJob . unwrapObject $ getObjValue "job" obj
            "nodeset" -> Just . ZNodeset . decodeNodeset . unwrapObject $ getObjValue "nodeset" obj
            "project" -> Just . ZProjectPipeline . decodeProjectPipeline . unwrapObject $ getObjValue "project" obj
            -- We use the same decoder as for "project" as the structure is slightly the same
            "project-template" -> Just . ZProjectTemplate . decodeProjectPipeline . unwrapObject $ getObjValue "project-template" obj
            _ -> Nothing
    decodeJob :: Object -> Job
    decodeJob va = case HM.lookup "name" va of
      Just (String name) -> do
        let jobName = JobName name
            ( jobParent,
              jobNodeset,
              jobBranches,
              jobDependencies
              ) = decodeJobContent va
         in Job {..}
      _ -> error $ "Unexpected job structure w/o name: " <> show va
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
        decodeJobBranches = decodeSimple "branches" BranchName va
        decodeJobDependencies :: [JobName]
        decodeJobDependencies = decodeSimple "dependencies" JobName va

    decodeNodeset :: Object -> Nodeset
    decodeNodeset va =
      let nodesetName = NodesetName . getString $ getObjValue "name" va
          nodesetLabels = case getObjValue "nodes" va of
            Array nodes ->
              NodeLabelName . getString . getObjValue "label"
                <$> (unwrapObject <$> V.toList nodes)
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
          pipelineTemplates = decodeSimple "templates" TemplateName va
          pipelinePipelines = catMaybes $ decodePipeline <$> sort (HM.toList va)
       in ProjectPipeline {..}
      where
        decodePipeline :: (Text, Value) -> Maybe Pipeline
        decodePipeline (pipelineName', va') = case va' of
          Object inner ->
            let pipelineName = PipelineName pipelineName'
                pipelineJobs = case HM.lookup "jobs" inner of
                  Just (String name) -> [PJName $ JobName name]
                  Just (Array jobElems) -> decodePipelineJob <$> sort (V.toList jobElems)
                  _ -> error $ "Unexpected project pipeline format: " <> show inner
             in Just $ Pipeline {..}
          _ -> Nothing
        decodePipelineJob :: Value -> PipelineJob
        decodePipelineJob jobElem = case jobElem of
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
    decodeSimple :: Text -> (Text -> a) -> Object -> [a]
    decodeSimple k build va = case HM.lookup k va of
      Just (String template) -> [build template]
      Just (Array templates) -> build . getString <$> sort (V.toList templates)
      Just _va -> error $ "Unexpected " <> T.unpack k <> " structure: " <> show _va
      Nothing -> []

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
            over projectConfigJobsL (insert (jobName job) job) projectConfig
        ZNodeset node ->
          updateProjectConfig xs $
            over projectConfigNodesetsL (insert (nodesetName node) node) projectConfig
        ZProjectPipeline project ->
          updateProjectConfig xs $
            over projectConfigProjectsL (insert (pName project) project) projectConfig
        _ -> updateProjectConfig xs projectConfig

emptyConfig :: Config
emptyConfig = Config mempty mempty

emptyProjectConfig :: ProjectConfig
emptyProjectConfig = ProjectConfig mempty mempty mempty
