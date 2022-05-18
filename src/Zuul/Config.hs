-- | The Zuul Configuration data types
-- See: https://zuul-ci.org/docs/zuul/latest/project-config.html#configuration-items
module Zuul.Config
  ( -- * Newtype wrapper
    TenantName (..),
    JobName (..),
    ProjectName (..),
    PipelineName (..),
    NodesetName (..),
    NodeLabelName (..),
    ProjectTemplateName (..),

    -- * Project identifier
    BranchName (..),
    ProviderName (..),
    ConnectionName (..),
    CanonicalProjectName (..),

    -- * Configuration data type
    Job (..),
    JobNodeset (..),
    Project (..),
    ProjectPipeline (..),
    PipelineJob (..),
    PipelineTrigger (..),
    Pipeline (..),
    Nodeset (..),
    ProjectTemplate (..),

    -- * Configuration identifier
    ConfigLoc (..),
    ZuulConfigElement (..),
    ZuulConfigType (..),
  )
where

import Data.Text.Lazy.Builder qualified as TB
import ZuulWeeder.Prelude

newtype BranchName = BranchName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

newtype JobName = JobName Text
  deriving (Eq, Ord, Show, Generic)
  deriving (Display) via (ShowInstance JobName)
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
  deriving (Display) via (ShowInstance NodesetName)

newtype NodeLabelName = NodeLabelName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)
  deriving (Display) via (ShowInstance NodeLabelName)

newtype ProviderName = ProviderName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

newtype ProjectTemplateName = ProjectTemplateName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

newtype CanonicalProjectName = CanonicalProjectName (ProviderName, ProjectName)
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

instance Display CanonicalProjectName where
  displayBuilder (CanonicalProjectName (_, ProjectName projectName)) =
    TB.fromText projectName

newtype ConnectionName = ConnectionName Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

newtype TenantName = TenantName {getName :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Hashable)

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

instance Display ZuulConfigElement where
  displayBuilder zce = case zce of
    ZJob job -> displayBuilder $ job.name
    ZNodeset ns -> displayBuilder $ ns.name
    _ -> error "display instance todo"

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

data ConfigLoc = ConfigLoc
  { project :: CanonicalProjectName,
    branch :: BranchName,
    path :: FilePathT,
    tenants :: [TenantName]
  }
  deriving (Show, Eq, Ord, Generic)

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
