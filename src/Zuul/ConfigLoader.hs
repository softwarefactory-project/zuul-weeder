{-# LANGUAGE FlexibleContexts #-}

-- |
module Zuul.ConfigLoader where

import Control.Lens (Lens', lens, over)
import Control.Monad.State
import Data.Aeson (Object, Value (Array, Object, String))
import qualified Data.HashMap.Strict as HM (keys, lookup)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Vector as V
import Zuul.ZKDump (ConfigError (..), ZKConfig (..))

newtype BranchName = BranchName Text deriving (Eq, Ord, Show)

newtype JobName = JobName Text deriving (Eq, Ord, Show)

newtype PipelineName = PipelineName Text deriving (Eq, Ord, Show)

data Job = Job {jobName :: JobName, parent :: Maybe JobName} deriving (Show, Eq)

data Pipeline = Pipeline {pipelineName :: PipelineName, job :: JobName} deriving (Show, Eq)

data ZuulConfigElement = ZJob Job | ZPipeline Pipeline deriving (Show, Eq)

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
        Array vec -> vec
        _ -> error $ "Unexpected root data structure on: " <> show rootObjs
   in V.toList $ V.mapMaybe getConfigElement rootObjs
  where
    getConfigElement :: Value -> Maybe ZuulConfigElement
    getConfigElement rootObj = case unwrapObject rootObj of
      -- Zuul config elements are object with an unique key
      obj -> case take 1 $ HM.keys obj of
        ["job"] -> ZJob . decodeJob . unwrapObject <$> HM.lookup "job" obj
        _ -> Nothing
    decodeJob :: Object -> Job
    decodeJob va = case HM.lookup "name" va of
      Just (String name) -> Job (JobName name) $ case HM.lookup "parent" va of
        Just (String parent) -> Just $ JobName parent
        _ -> Nothing
      _ -> error $ "Unexpected job structure w/o name: " <> show va
    unwrapObject :: Value -> Object
    unwrapObject va = case va of
      Object hm -> hm
      _ -> error $ "Expecting an Object out of JSON Value: " <> show va

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
