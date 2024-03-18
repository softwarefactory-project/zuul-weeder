-- |
-- Module      : ZuulWeeder.UI.Vertex
-- Description : The Vertex attributes
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
module ZuulWeeder.UI.Vertex (vertexSlugName, vertexIcon, configLocUrl, projectUrl) where

import Data.Text qualified as Text
import Lucid
import Zuul.Config
import ZuulWeeder.Graph
import ZuulWeeder.Prelude
import ZuulWeeder.UI.CSS

-- | A text representation of a vertex type, useful for /object url piece.
vertexSlugName :: VertexType -> Text
vertexSlugName = \case
  VAbstractJobT -> "abstract-job"
  VJobT -> "job"
  VSemaphoreT -> "semaphore"
  VSecretT -> "secret"
  VNodesetT -> "nodeset"
  VNodeLabelT -> "label"
  VQueueT -> "queue"
  VProjectT -> "project-config"
  VProjectRegexT -> "project-regex"
  VProjectTemplateT -> "project-template"
  VPipelineT -> "pipeline"
  VProjectPipelineT -> "project-pipeline"
  VRegexPipelineT -> "regex-pipeline"
  VTemplatePipelineT -> "template-pipeline"
  VRepositoryT -> "repository"
  VTriggerT -> "trigger"
  VReporterT -> "reporter"

vertexIcon :: VertexType -> Html ()
vertexIcon vt = mkIcon (Just $ vertexSlugName vt) ("ri-" <> iconName)
  where
    iconName = case vt of
      VAbstractJobT -> "file-text-line"
      VJobT -> "file-text-line"
      VSemaphoreT -> "lock-line"
      VSecretT -> "key-2-line"
      VQueueT -> "traffic-light-line"
      VProjectT -> "folder-open-line"
      VProjectRegexT -> "folder-open-line"
      VProjectTemplateT -> "draft-line"
      VPipelineT -> "git-merge-line"
      VNodeLabelT -> "price-tag-3-line"
      VProjectPipelineT -> "git-merge-line"
      VRegexPipelineT -> "git-merge-line"
      VTemplatePipelineT -> "git-merge-line"
      VNodesetT -> "server-line"
      VRepositoryT -> "stack-line"
      VTriggerT -> "download-fill"
      VReporterT -> "upload-fill"

-- | Get the URL of a configuration element location
configLocUrl :: ConfigLoc -> Text
configLocUrl loc = case loc.url of
  GerritUrl (trimedUrl -> url)
    | url == "https://review.opendev.org" -> buildGiteaUrl "https://opendev.org"
    | otherwise -> url <> "/plugins/gitiles/" <> name <> "/+/refs/heads/" <> branch <> "/" <> path
  GithubUrl url -> buildGithubUrl url
  GitlabUrl url -> buildGitlabUrl url
  PagureUrl url -> buildPagureUrl url
  GitUrl url
    | "gitlab" `Text.isInfixOf` url -> buildGitlabUrl url
    | "github.com" `Text.isInfixOf` url -> buildGithubUrl url
    | "pagure.io" `Text.isInfixOf` url -> buildPagureUrl url
    | "src.fedoraproject.io" `Text.isInfixOf` url -> buildPagureUrl url
  GitUrl url -> trimedUrl url <> "/cgit/" <> name <> "/tree/" <> path <> "?h=" <> branch
  where
    CanonicalProjectName _ (ProjectName name) = loc.project
    BranchName branch = loc.branch
    FilePathT path = loc.path
    trimedUrl = Text.dropWhileEnd (== '/')
    buildGitlabUrl url = trimedUrl url <> "/" <> name <> "/-/blob/" <> branch <> "/" <> path
    buildPagureUrl url = trimedUrl url <> "/" <> name <> "/blob/" <> branch <> "/f/" <> path
    buildGithubUrl url = trimedUrl url <> "/" <> name <> "/blob/" <> branch <> "/" <> path
    buildGiteaUrl url = url <> "/" <> name <> "/src/branch/" <> branch <> "/" <> path

projectUrl :: ConnectionUrl -> CanonicalProjectName -> Text
projectUrl curl proj = case curl of
  GerritUrl (trimedUrl -> url)
    | url == "https://review.opendev.org" -> buildGiteaUrl "https://opendev.org"
    | otherwise -> url <> "/plugins/gitiles/" <> name
  GithubUrl url -> buildGithubUrl url
  GitlabUrl url -> buildGitlabUrl url
  PagureUrl url -> buildPagureUrl url
  GitUrl url
    | "gitlab" `Text.isInfixOf` url -> buildGitlabUrl url
    | "github.com" `Text.isInfixOf` url -> buildGithubUrl url
    | "pagure.io" `Text.isInfixOf` url -> buildPagureUrl url
    | "src.fedoraproject.io" `Text.isInfixOf` url -> buildPagureUrl url
  GitUrl url -> trimedUrl url <> "/cgit/" <> name
  where
    CanonicalProjectName _ (ProjectName name) = proj
    trimedUrl = Text.dropWhileEnd (== '/')
    buildGitlabUrl url = trimedUrl url <> "/" <> name
    buildPagureUrl url = trimedUrl url <> "/" <> name
    buildGithubUrl url = trimedUrl url <> "/" <> name
    buildGiteaUrl url = url <> "/" <> name
