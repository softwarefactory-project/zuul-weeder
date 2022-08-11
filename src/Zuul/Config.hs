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
    ProjectRegex (..),
    PipelineName (..),
    NodesetName (..),
    NodeLabelName (..),
    ProjectTemplateName (..),
    SecretName (..),
    QueueName (..),
    SemaphoreName (..),

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
    PipelineReporter (..),
    Pipeline (..),
    Nodeset (..),
    ProjectTemplate (..),

    -- * Configuration identifiers
    BaseConfigLoc (..),
    ConfigLoc,
    ZuulConfigElement (..),
    ZuulConfigType (..),
  )
where

import ZuulWeeder.Prelude

newtype BranchName = BranchName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, FromJSON, ToJSON)

newtype JobName = JobName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

newtype PipelineName = PipelineName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

newtype ProjectName = ProjectName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

instance From ProjectName Text where
  from (ProjectName n) = n

newtype ProjectRegex = ProjectRegex Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

instance From ProjectName ProjectRegex where
  from (ProjectName n) = ProjectRegex n

newtype NodesetName = NodesetName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

newtype NodeLabelName = NodeLabelName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

newtype ProviderName = ProviderName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

newtype ProjectTemplateName = ProjectTemplateName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

newtype SecretName = SecretName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

newtype QueueName = QueueName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

newtype SemaphoreName = SemaphoreName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data CanonicalProjectName = CanonicalProjectName
  { provider :: ProviderName,
    project :: ProjectName
  }
  deriving (Eq, Ord, Show, Generic, Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

instance From CanonicalProjectName Text where
  from (CanonicalProjectName (ProviderName p) (ProjectName n)) = p <> "/" <> n

newtype ConnectionName = ConnectionName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

-- TODO: use Network.URI.URI instead of Text

-- | The sum of all the possible connection urls
data ConnectionUrl
  = GerritUrl Text
  | GitlabUrl Text
  | GithubUrl Text
  | PagureUrl Text
  | GitUrl Text
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype TenantName = TenantName Text
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Hashable, FromJSON, ToJSON)

instance From TenantName Text where
  from (TenantName name) = name

data JobNodeset
  = JobNodeset NodesetName
  | JobAnonymousNodeset [NodeLabelName]
  deriving (Eq, Ord, Show, Generic, Hashable, FromJSON, ToJSON)

data Nodeset = Nodeset
  { name :: NodesetName,
    labels :: [NodeLabelName]
  }
  deriving (Show, Eq, Ord, Generic, Hashable, FromJSON, ToJSON)

data Job = Job
  { name :: JobName,
    abstract :: Maybe Bool,
    parent :: Maybe JobName,
    nodeset :: Maybe JobNodeset,
    branches :: Maybe [BranchName],
    dependencies :: Maybe [JobName],
    requiredProjects :: Maybe [ProjectName],
    requiredCanonicalProjects :: Maybe [CanonicalProjectName],
    semaphores :: Maybe [SemaphoreName],
    secrets :: Maybe [SecretName]
  }
  deriving (Show, Eq, Ord, Generic, Hashable)

instance FromJSON Job where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance ToJSON Job where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

data PipelineJob
  = PJName JobName
  | PJJob Job
  deriving (Show, Eq, Ord, Generic, Hashable, FromJSON, ToJSON)

instance From PipelineJob JobName where
  from = \case
    PJName name -> name
    PJJob job -> job.name

data ProjectPipeline = ProjectPipeline
  { name :: PipelineName,
    jobs :: [PipelineJob]
  }
  deriving (Show, Eq, Ord, Generic, Hashable, FromJSON, ToJSON)

data Project = Project
  { name :: ProjectName,
    templates :: [ProjectTemplateName],
    queue :: Maybe QueueName,
    pipelines :: Set ProjectPipeline
  }
  deriving (Show, Eq, Ord, Generic, Hashable, FromJSON, ToJSON)

data ProjectTemplate = ProjectTemplate
  { name :: ProjectTemplateName,
    queue :: Maybe QueueName,
    pipelines :: Set ProjectPipeline
  }
  deriving (Show, Eq, Ord, Generic, Hashable, FromJSON, ToJSON)

newtype PipelineTrigger = PipelineTrigger {connectionName :: ConnectionName}
  deriving (Show, Ord, Eq, Generic)
  deriving newtype (Hashable, FromJSON, ToJSON)

newtype PipelineReporter = PipelineReporter {connectionName :: ConnectionName}
  deriving (Show, Ord, Eq, Generic)
  deriving newtype (Hashable, FromJSON, ToJSON)

data Pipeline = Pipeline
  { name :: PipelineName,
    triggers :: [PipelineTrigger],
    reporters :: [PipelineReporter]
  }
  deriving (Show, Eq, Ord, Generic, Hashable, FromJSON, ToJSON)

-- | The sum of all the configuration elements.
data ZuulConfigElement
  = ZJob Job
  | ZProject Project
  | ZNodeset Nodeset
  | ZProjectTemplate ProjectTemplate
  | ZPipeline Pipeline
  | ZQueue QueueName
  | ZSemaphore SemaphoreName
  | ZSecret SecretName
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance From ZuulConfigElement ZuulConfigType where
  from zce = case zce of
    ZJob _ -> JobT
    ZProject _ -> ProjectT
    ZNodeset _ -> NodesetT
    ZProjectTemplate _ -> ProjectTemplateT
    ZPipeline _ -> PipelineT
    ZQueue _ -> QueueT
    ZSemaphore _ -> SemaphoreT
    ZSecret _ -> SecretT

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
  deriving (Show, Eq, Ord, Generic, Enum, Bounded, FromJSON, ToJSON)

-- | The configuration source context location
data BaseConfigLoc resolved = BaseConfigLoc
  { project :: CanonicalProjectName,
    branch :: BranchName,
    path :: FilePathT,
    url :: ConnectionUrl,
    tenants :: Set resolved
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | A resolved config location, where the owning tenants have been resolved.
type ConfigLoc = BaseConfigLoc TenantName

instance From ConfigLoc CanonicalProjectName where
  from loc = loc.project
