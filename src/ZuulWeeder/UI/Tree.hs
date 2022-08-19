-- |
-- Module      : ZuulWeeder.UI.Tree
-- Description : The tree component
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
module ZuulWeeder.UI.Tree (treeComponent) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Lucid
import Zuul.Config
import Zuul.ConfigLoader (Config (..))
import ZuulWeeder.Graph
import ZuulWeeder.Prelude
import ZuulWeeder.UI
import ZuulWeeder.UI.CSS
import ZuulWeeder.UI.Vertex

treeComponent :: Context -> NonEmpty Vertex -> Analysis -> Html ()
treeComponent ctx vertices analysis = do
  h2_ do
    vertexIcon (from vertex.name)
    toHtml (from vertex.name :: Text)
  ul_ do
    traverse_ li_ configComponents
  with' div_ "grid grid-cols-2 gap-1 m-4" do
    div_ do
      unless (null dependents) do
        titleWithTooltip ("The dependents list require '" <> from vertex.name <> "'") "Dependents"
        traverse_ (renderTree ctx Dependent vertex) dependents
    div_ do
      unless (null dependencies) do
        titleWithTooltip ("'" <> from vertex.name <> "' requires the dependency list") "Dependencies"
        traverse_ (renderTree ctx Dependency vertex) dependencies

      unless (null owned) do
        titleWithTooltip ("'" <> from vertex.name <> "' provides") "Provides"
        with' ul_ "pl-2" do
          traverse_ (\v -> li_ $ vertexLink ctx v.name (vertexName v.name)) owned
  script_ do
    "addTreeHandler()"
  where
    owned :: [Vertex]
    owned = case vertex.name of
      VRepository n -> case Map.lookup n analysis.repositoryContent of
        Just objects -> sort $ Set.toList objects
        Nothing -> []
      _ -> []

    renderConfigLink :: Maybe (Html ()) -> ConfigLoc -> Html ()
    renderConfigLink extra loc = do
      case extra of
        Just h -> h
        Nothing -> pure ()
      with div_ [title_ $ "'" <> from vertex.name <> "' is provided by this repo"] do
        let vRepo = VRepository loc.project
        vertexLink ctx vRepo (vertexName vRepo)
        traverse_ (tenantLink ctx.rootURL vertex.name) loc.tenants
      with div_ [title_ $ "'" <> from vertex.name <> "' is defined at this url"] do
        locLink $ configLocUrl loc

    renderProjectLink :: ConnectionUrl -> CanonicalProjectName -> Set TenantName -> Html ()
    renderProjectLink url project tenants = do
      locLink $ projectUrl url project
      traverse_ (tenantLink ctx.rootURL vertex.name) tenants

    vertex = NE.head vertices
    forTenant :: Set TenantName -> Bool
    forTenant locTenants = case ctx.scope of
      Scoped tenants -> tenants `Set.isSubsetOf` locTenants
      UnScoped -> True

    getLocs :: Maybe [(ConfigLoc, a)] -> [Html ()]
    getLocs = map (renderConfigLink Nothing) . filter (forTenant . (.tenants)) . maybe [] (fmap fst)

    renderPipelineInfo :: Pipeline -> Html ()
    renderPipelineInfo p = traverse_ (with div_ [title_ "Trigger frequency"] . toHtml) p.timers

    getPipelineLocs :: Maybe [(ConfigLoc, Pipeline)] -> [Html ()]
    getPipelineLocs =
      map (\(loc, pipeline) -> renderConfigLink (Just $ renderPipelineInfo pipeline) loc)
        . filter (forTenant . (.tenants) . fst)
        . fromMaybe []

    configComponents :: [Html ()]
    configComponents = case vertex.name of
      VAbstractJob name -> getLocs $ Map.lookup name analysis.config.jobs
      VJob name -> getLocs $ Map.lookup name analysis.config.jobs
      VSecret name -> getLocs $ Map.lookup name analysis.config.secrets
      VSemaphore name -> getLocs $ Map.lookup name analysis.config.semaphores
      VQueue name -> getLocs $ Map.lookup name analysis.config.queues
      VProject name -> getLocs $ Map.lookup name analysis.config.projects
      VProjectRegex name -> getLocs $ Map.lookup name analysis.config.projectRegexs
      VProjectTemplate name -> getLocs $ Map.lookup name analysis.config.projectTemplates
      VPipeline name -> getPipelineLocs $ Map.lookup name analysis.config.pipelines
      VNodeset name -> getLocs $ Map.lookup name analysis.config.nodesets
      VNodeLabel name -> getLocs $ Map.lookup name analysis.config.nodeLabels
      VProjectPipeline _ name -> getLocs $ Map.lookup name analysis.config.projects
      VRegexPipeline _ name -> getLocs $ Map.lookup name analysis.config.projectRegexs
      VTemplatePipeline _ name -> getLocs $ Map.lookup name analysis.config.projectTemplates
      VRepository name -> case Map.lookup name analysis.config.canonicalProjects of
        Just tenants | forTenant tenants -> case Map.lookup name.provider analysis.config.urlBuilder of
          Just cu -> [renderProjectLink cu name tenants] -- Left (cu, name, tenants)]
          _ -> []
        _ -> []
      VTrigger name -> getLocs $ Map.lookup name analysis.config.triggers
      VReporter name -> getLocs $ Map.lookup name analysis.config.reporters
    dependencies = getForest analysis.dependencyMap
    dependents = getForest analysis.dependentMap
    getForest = ZuulWeeder.Graph.findReachableForest tenantsM vertices
      where
        tenantsM = case ctx.scope of
          UnScoped -> Nothing
          Scoped xs -> Just xs

data TreeDirection = Dependent | Dependency deriving (Eq)

renderTree :: Context -> TreeDirection -> Vertex -> Tree VertexName -> Html ()
renderTree ctx treeDirection rootVertex = go (0 :: Int) Nothing
  where
    isPpc = isJust $ isPipelineConfig (rootVertex.name)
    isDependent = treeDirection == Dependent

    -- This function defines when to stop a tree representation
    stopTree :: Maybe VertexName -> VertexName -> Bool
    stopTree parent root = stopParent || stopPipelineJob || stopProjectPipelineJob
      where
        -- Hide jobs on the Dependents list when their parent are a pipeline config.
        -- This is because if a job is needed by a pipeline config, then we don't want
        -- to see all the jobs needed by that pipeline config
        isParentPipeline = maybe False (isJust . isPipelineConfig . from) parent
        stopPipelineJob =
          isDependent && isParentPipeline && case from root of
            VJobT -> True
            _ -> False

        -- Hide jobs on the Dependent list when looking at a project pipeline config
        -- This is because relevant jobs are already in the dependency list
        stopProjectPipelineJob =
          isDependent && isPpc && case from root of
            VJobT -> True
            _ -> False

        -- Stop the tree after we reach a job or a project.
        stopParent = maybe False (whenParent . from) parent
        whenParent = \case
          VJobT -> True
          VAbstractJobT -> True
          VProjectT -> True
          _ -> False

    go depth parent (Node root childs)
      | stopTree parent root = pure ()
      | otherwise = do
          let listStyle
                | depth > 0 = "pl-2 border-solid rounded border-l-2 border-slate-500"
                | otherwise = ""
              isNested = depth > 2
              showCarret = not (List.null childs) && depth > 1
              nestedStyle
                | isNested = " nested"
                | otherwise = ""
          with' ul_ (listStyle <> nestedStyle) do
            li_ do
              when showCarret do
                with' span_ "tree-caret" mempty
              vertexLink ctx root (vertexName root)
              unless (List.null childs) do
                traverse_ (go (depth + 1) (Just root)) childs
