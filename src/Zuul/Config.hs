-- |
-- Module      : Zuul.Config
-- Description : Configuration data types
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
--
-- Maintainer  : tdecacqu@redhat.com, fboucher@redhat.com
-- Stability   : provisional
-- Portability : portable
--
-- The Zuul Configuration data types.
--
-- See: https://zuul-ci.org/docs/zuul/latest/project-config.html#configuration-items
module Zuul.Config
  ( -- * Newtype wrappers
    TenantName (..),
    JobName (..),
    ProjectName (..),
    PipelineName (..),
    NodesetName (..),
    NodeLabelName (..),
    ProjectTemplateName (..),

    -- * Project identifiers
    BranchName (..),
    ProviderName (..),
    ConnectionName (..),
    ConnectionUrl (..),
    CanonicalProjectName (..),

    -- * Configuration data types
    Job (..),
    JobNodeset (..),
    Project (..),
    ProjectPipeline (..),
    PipelineJob (..),
    PipelineTrigger (..),
    Pipeline (..),
    Nodeset (..),
    ProjectTemplate (..),

    -- * Configuration identifiers
    ConfigLoc (..),
    ZuulConfigElement (..),
    ZuulConfigType (..),
  )
where

import ZuulWeeder.Prelude

newtype BranchName = BranchName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

newtype JobName = JobName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

newtype PipelineName = PipelineName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

newtype ProjectName = ProjectName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

newtype NodesetName = NodesetName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

newtype NodeLabelName = NodeLabelName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

newtype ProviderName = ProviderName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

newtype ProjectTemplateName = ProjectTemplateName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

data CanonicalProjectName = CanonicalProjectName
  { provider :: ProviderName,
    project :: ProjectName
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

newtype ConnectionName = ConnectionName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

-- TODO: use Network.URI.URI instead of Text

-- | The sum of all the possible connection urls
data ConnectionUrl
  = GerritUrl Text
  | GitlabUrl Text
  | GithubUrl Text
  | PagureUrl Text
  | GitUrl Text
  deriving (Eq, Ord, Show)

newtype TenantName = TenantName Text
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Hashable)

instance From TenantName Text where
  from (TenantName name) = name

data JobNodeset
  = JobNodeset NodesetName
  | JobAnonymousNodeset [NodeLabelName]
  deriving (Eq, Ord, Show, Generic, Hashable)

data Nodeset = Nodeset
  { name :: NodesetName,
    labels :: [NodeLabelName]
  }
  deriving (Show, Eq, Ord, Generic, Hashable)

data Job = Job
  { name :: JobName,
    parent :: Maybe JobName,
    nodeset :: Maybe JobNodeset,
    branches :: [BranchName],
    dependencies :: [JobName]
  }
  deriving (Show, Eq, Ord, Generic, Hashable)

data PipelineJob
  = PJName JobName
  | PJJob Job
  deriving (Show, Eq, Ord, Generic, Hashable)

data ProjectPipeline = ProjectPipeline
  { name :: PipelineName,
    jobs :: [PipelineJob]
  }
  deriving (Show, Eq, Ord, Generic, Hashable)

data Project = Project
  { name :: ProjectName,
    templates :: [ProjectTemplateName],
    pipelines :: Set ProjectPipeline
  }
  deriving (Show, Eq, Ord, Generic, Hashable)

data ProjectTemplate = ProjectTemplate
  { name :: ProjectTemplateName,
    pipelines :: Set ProjectPipeline
  }
  deriving (Show, Eq, Ord, Generic, Hashable)

newtype PipelineTrigger = PipelineTrigger {connectionName :: ConnectionName}
  deriving (Show, Ord, Eq, Generic)
  deriving newtype (Hashable)

data Pipeline = Pipeline
  { name :: PipelineName,
    triggers :: [PipelineTrigger]
  }
  deriving (Show, Eq, Ord, Generic, Hashable)

data Queue = Queue {name :: Text, perBranch :: Bool}
  deriving (Show, Eq, Ord, Generic, Hashable)

data Semaphore = Semaphore {name :: Text, max :: Int}
  deriving (Show, Eq, Ord, Generic, Hashable)

-- | The sum of all the configuration elements.
data ZuulConfigElement
  = ZJob Job
  | ZProject Project
  | ZNodeset Nodeset
  | ZProjectTemplate ProjectTemplate
  | ZPipeline Pipeline
  | ZQueue Queue
  | ZSemaphore Semaphore
  deriving (Show, Eq, Ord)

instance From ZuulConfigElement ZuulConfigType where
  from zce = case zce of
    ZJob _ -> JobT
    ZProject _ -> ProjectT
    ZNodeset _ -> NodesetT
    ZProjectTemplate _ -> ProjectTemplateT
    ZPipeline _ -> PipelineT
    ZQueue _ -> QueueT
    ZSemaphore _ -> SemaphoreT

-- | The sum of all the configuration types.
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

-- | The configuration source context location
data ConfigLoc = ConfigLoc
  { project :: CanonicalProjectName,
    branch :: BranchName,
    path :: FilePathT,
    url :: ConnectionUrl,
    tenants :: Set TenantName
  }
  deriving (Show, Eq, Ord, Generic)
