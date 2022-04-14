{-# LANGUAGE FlexibleContexts #-}

-- |
module Zuul.ConfigLoader where

import Control.Lens (Lens', lens, over)
import Control.Monad.State
import Data.Aeson (Object, Value (Array, Object, String))
import qualified Data.HashMap.Strict as HM (keys, lookup, toList)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Zuul.ZKDump (ConfigError (..), ZKConfig (..))

newtype BranchName = BranchName Text deriving (Eq, Ord, Show)

newtype JobName = JobName Text deriving (Eq, Ord, Show)

newtype PipelineName = PipelineName Text deriving (Eq, Ord, Show)

newtype ProjectName = ProjectName Text deriving (Eq, Ord, Show)

newtype NodesetName = NodesetName Text deriving (Eq, Ord, Show)

newtype NodeLabelName = NodeLabelName Text deriving (Eq, Ord, Show)

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
    nodeset :: Maybe JobNodeset
  }
  deriving (Show, Eq, Ord)

data Pipeline = Pipeline
  { pipelineName :: PipelineName,
    job :: JobName
  }
  deriving (Show, Eq, Ord)

data ProjectPipeline = ProjectPipeline
  { projectName :: ProjectName,
    pPipelineName :: PipelineName,
    pipelineJobs :: [JobName]
  }
  deriving (Show, Eq, Ord)

data ZuulConfigElement
  = ZJob Job
  | ZPipeline Pipeline
  | ZProjectPipeline ProjectPipeline
  | ZNodeset Nodeset
  deriving (Show, Eq, Ord)

data Config = Config
  { jobs :: Map (BranchName, JobName) Job,
    pipelines :: Map PipelineName Pipeline,
    configError :: [ConfigError]
  }
  deriving (Show)

configErrorL :: Lens' Config [ConfigError]
configErrorL = lens configError (\c ce -> c {configError = ce})

decodeConfig :: Value -> [ZuulConfigElement]
decodeConfig zkJSONData =
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
          _ -> error $ "Unexpected nodeset structure: " <> show va
    decodeNodeset :: Object -> Nodeset
    decodeNodeset va =
      let nodesetName = NodesetName . getString $ getObjValue "name" va
          nodesetLabels = case getObjValue "nodes" va of
            Array nodes ->
              NodeLabelName . getString . getObjValue "label"
                <$> (unwrapObject <$> V.toList nodes)
            _ -> error $ "Unexpected nodeset nodes structure: " <> show va
       in Nodeset {..}

    decodeProjectPipeline :: Object -> [ProjectPipeline]
    decodeProjectPipeline va =
      -- TODO: need file context to deduce the pipeline name
      let name = maybe "" getString (HM.lookup "name" va)
       in ( \(pName, jobs) ->
              ProjectPipeline (ProjectName name) (PipelineName pName) jobs
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

loadConfig :: Either ConfigError ZKConfig -> StateT Config IO ()
loadConfig zkc = do
  liftIO $ putStrLn $ "Loading " <> show zkc
  case zkc of
    Left e -> modify (addError e)
    Right _x -> pure ()
  where
    addError :: ConfigError -> Config -> Config
    addError ce = configErrorL `over` (ce :)

emptyConfig :: Config
emptyConfig = Config mempty mempty mempty
