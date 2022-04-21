{-# LANGUAGE FlexibleContexts #-}

-- |
module Zuul.ConfigLoader where

import Control.Lens (Lens', lens, over, view)
import Control.Monad.State
import Data.Aeson (Object, Value (Array, Object, String))
import qualified Data.HashMap.Strict as HM (keys, lookup, toList)
import Data.Map (Map, insert, lookup)
import Data.Maybe (fromMaybe, mapMaybe)
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
    parent :: Maybe JobName,
    nodeset :: Maybe JobNodeset,
    branches :: [BranchName]
  }
  deriving (Show, Eq, Ord)

data Pipeline = Pipeline
  { pipelineName :: PipelineName,
    job :: JobName
  }
  deriving (Show, Eq, Ord)

data ProjectPipeline = ProjectPipeline
  { projectName :: Project,
    pPipelineName :: PipelineName,
    pipelineTemplates :: [TemplateName],
    pipelineJobs :: [JobName]
  }
  deriving (Show, Eq, Ord)

data ZuulConfigElement
  = ZJob Job
  | ZPipeline Pipeline
  | ZProjectPipeline ProjectPipeline
  | ZNodeset Nodeset
  deriving (Show, Eq, Ord)

data ProjectConfig = ProjectConfig
  { jobs :: Map JobName Job,
    pipelines :: Map PipelineName Pipeline,
    nodesets :: Map NodesetName Nodeset
  }
  deriving (Show)

configProviderNodesetsL :: Lens' ProjectConfig (Map NodesetName Nodeset)
configProviderNodesetsL = lens nodesets (\c nodesets -> c {nodesets = nodesets})

configProviderJobsL :: Lens' ProjectConfig (Map JobName Job)
configProviderJobsL = lens jobs (\c jobs -> c {jobs = jobs})

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
   in concatMap getConfigElement rootObjs
  where
    getConfigElement :: Value -> [ZuulConfigElement]
    getConfigElement rootObj = case unwrapObject rootObj of
      obj -> case getKey obj of
        "job" -> (: []) . ZJob . decodeJob . unwrapObject $ getObjValue "job" obj
        "project" -> ZProjectPipeline <$> (decodeProjectPipeline . unwrapObject $ getObjValue "project" obj)
        "nodeset" -> (: []) . ZNodeset . decodeNodeset . unwrapObject $ getObjValue "nodeset" obj
        _ -> []
    decodeJob :: Object -> Job
    decodeJob va = case HM.lookup "name" va of
      Just (String name) -> do
        let jobName = JobName name
            parent = case HM.lookup "parent" va of
              Just (String p) -> Just $ JobName p
              _ -> Nothing
            nodeset = decodeJobNodeset
            branches = decodeJobBranches
         in Job {..}
      _ -> error $ "Unexpected job structure w/o name: " <> show va
      where
        decodeJobNodeset :: Maybe JobNodeset
        decodeJobNodeset = case HM.lookup "nodeset" va of
          Just (String n) -> Just . JobNodeset $ NodesetName n
          Just (Object nObj) -> case getObjValue "nodes" nObj of
            Array nodes ->
              Just $
                JobAnonymousNodeset $
                  NodeLabelName . getString . getObjValue "label"
                    <$> (unwrapObject <$> V.toList nodes)
            _ -> error $ "Unexpected nodeset nodes structure: " <> show nObj
          Just _va -> error $ "Unexpected nodeset structure: " <> show _va
          Nothing -> Nothing
        decodeJobBranches :: [BranchName]
        decodeJobBranches = decodeSimple "branches" BranchName va

    decodeNodeset :: Object -> Nodeset
    decodeNodeset va =
      let nodesetName = NodesetName . getString $ getObjValue "name" va
          nodesetLabels = case getObjValue "nodes" va of
            Array nodes ->
              NodeLabelName . getString . getObjValue "label"
                <$> (unwrapObject <$> V.toList nodes)
            _va -> error $ "Unexpected nodeset nodes structure: " <> show _va
       in Nodeset {..}

    decodeProjectPipeline :: Object -> [ProjectPipeline]
    decodeProjectPipeline va =
      let projectName = case getString <$> HM.lookup "name" va of
            Nothing -> PNameCannonical project
            Just name -> case T.uncons name of
              Just ('^', _) -> PNameRE $ ProjectNameRE name
              Just _ -> PName $ ProjectName name
              Nothing -> error $ "Unexpected project name for project pipeline: " <> T.unpack name
       in ( \(pName, jobs) ->
              let pPipelineName = PipelineName pName
                  pipelineTemplates = decodeSimple "templates" TemplateName va
                  pipelineJobs = jobs
               in ProjectPipeline {..}
          )
            <$> mapMaybe getPipelineJobs (HM.toList va)
      where
        getPipelineJobs :: (Text, Value) -> Maybe (Text, [JobName])
        getPipelineJobs (pipelineName, va') = case va' of
          Object inner ->
            let jobs = case HM.lookup "jobs" inner of
                  Just (String name) -> [JobName name]
                  Just (Array jobElems) ->
                    ( \jobElem -> case jobElem of
                        Object jobObj -> JobName $ getKey jobObj
                        String v -> JobName v
                        _ -> error $ "Unexpected project pipeline jobs format: " <> show jobElem
                    )
                      <$> V.toList jobElems
                  _ -> error $ "Unexpected project pipeline format: " <> show inner
             in Just (pipelineName, jobs)
          _ -> Nothing
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
      Just (Array templates) -> build . getString <$> V.toList templates
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
            over configProviderJobsL (insert (jobName job) job) projectConfig
        ZNodeset node ->
          updateProjectConfig xs $
            over configProviderNodesetsL (insert (nodesetName node) node) projectConfig
        _ -> updateProjectConfig xs projectConfig

emptyConfig :: Config
emptyConfig = Config mempty mempty

emptyProjectConfig :: ProjectConfig
emptyProjectConfig = ProjectConfig mempty mempty mempty
