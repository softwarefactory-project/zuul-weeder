{-# LANGUAGE FlexibleContexts #-}

-- |
module Zuul.ConfigLoader where

import Control.Lens (Lens', lens, over)
import Control.Monad.State
import Data.Map (Map)
import Data.Text (Text)
import Zuul.ZKDump (ConfigError (..), ZKConfig (..))

newtype BranchName = BranchName Text deriving (Eq, Ord, Show)

newtype JobName = JobName Text deriving (Eq, Ord, Show)

newtype PipelineName = PipelineName Text deriving (Eq, Ord, Show)

data Job = Job {jobName :: JobName, parent :: Maybe JobName} deriving (Show)

data Pipeline = Pipeline {pipelineName :: PipelineName, job :: JobName} deriving (Show)

data Config = Config
  { jobs :: Map (BranchName, JobName) Job,
    pipelines :: Map PipelineName Pipeline,
    configError :: [ConfigError]
  }
  deriving (Show)

configErrorL :: Lens' Config [ConfigError]
configErrorL = lens configError (\c ce -> c {configError = ce})

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
