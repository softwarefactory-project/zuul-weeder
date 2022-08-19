-- |
-- Module      : ZuulWeeder.UI.Info
-- Description : The info page component
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
module ZuulWeeder.UI.Info (infoComponent) where

import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Lucid
import Zuul.Config
import Zuul.ConfigLoader (Config (..), ConfigMap)
import ZuulWeeder.Graph
import ZuulWeeder.Prelude
import ZuulWeeder.UI
import ZuulWeeder.UI.CSS
import ZuulWeeder.UI.Vertex

pipelineJobCount :: Context -> Analysis -> PipelineName -> Int
pipelineJobCount ctx analysis pipeline = sum $ map countJob pipelineForest
  where
    countJob :: Tree VertexName -> Int
    countJob (Node root childs)
      | isPipelineConfig root == Just pipeline = length $ filter isJob childs
      | otherwise = 0
    isJob :: Tree VertexName -> Bool
    isJob (Node root _) = isJobVertex root

    pipelineForest = case NE.nonEmpty pipelineVerticesList of
      Just pipelineVertices -> ZuulWeeder.Graph.findReachableForest tenantM pipelineVertices analysis.dependentMap
      Nothing -> mempty

    pipelineVerticesList = vertexScope ctx.scope $ Set.filter matchVertex analysis.vertices
      where
        matchVertex v = v.name == VPipeline pipeline

    tenantM = case ctx.scope of
      UnScoped -> Nothing
      Scoped tenants -> Just tenants

-- | Display the list of pipelines
pipelinesInfoComponent :: Context -> Analysis -> Zuul.ConfigLoader.ConfigMap PipelineName Pipeline -> Html ()
pipelinesInfoComponent ctx analysis pipelines = do
  title "Pipelines"
  ul_ do
    forM_ (Map.toList perLocs) $ \(repo, xs) -> do
      with' li_ "pt-2" do
        let vProj = VRepository repo
        with div_ [title_ $ "Pipelines defined in '" <> from vProj <> "'"] do
          vertexLink ctx vProj (vertexName vProj)
        with' ul_ "pl-2 pb-2" do
          traverse_ (with' li_ "pb-2" . displayPipeline) xs
  where
    jobCounts :: Map PipelineName Int
    jobCounts = Map.mapWithKey (\n _ -> pipelineJobCount ctx analysis n) pipelines
    perLocs :: Map CanonicalProjectName [Pipeline]
    perLocs = foldr addPipelines mempty (Map.elems pipelines)
    addPipelines xs acc = foldr addPipeline acc xs
    addPipeline (loc, pipeline)
      | forTenant loc = Map.insertWith mappend loc.project [pipeline]
      | otherwise = ZuulWeeder.Prelude.id

    forTenant :: ConfigLoc -> Bool
    forTenant loc = case ctx.scope of
      Scoped tenants -> loc.tenants `Set.isSubsetOf` tenants
      UnScoped -> True

    displayPipeline :: Pipeline -> Html ()
    displayPipeline pipeline = do
      let vPipeline = VPipeline pipeline.name
          jobCount = fromMaybe 0 $ Map.lookup pipeline.name jobCounts
      vertexLink ctx vPipeline do
        vertexName vPipeline
        with span_ [class_ "pl-2", title_ "Job count"] do
          toHtml $ show jobCount
          vertexIcon VJobT

      unless (null pipeline.timers) do
        with' ul_ "pl-2" do
          forM_ pipeline.timers $ \timer -> do
            li_ (toHtml timer)

infoComponent :: Context -> Analysis -> Html ()
infoComponent ctx analysis = do
  with' div_ "grid p-4 place-content-center" do
    with' span_ "font-semibold pb-3" do
      "Config details"
      traverse_ (tenantBaseLink ctx.rootURL) scope
    with' div_ "pb-3" do
      unless (Set.null otherTenants) $ do
        "Available tenants:"
        traverse_ (tenantInfoLink ctx.rootURL) otherTenants
    with' div_ "not-prose bg-slate-50 border rounded-xl w-80" do
      with' table_ "table-auto border-collapse w-80" do
        thead_ $ with' tr_ "border-b text-left" $ traverse_ (with' th_ "p-1") ["Object", "Count"]
        with' tbody_ "bg-white" do
          objectCounts "jobs" config.jobs
          objectCounts "nodesets" config.nodesets
          objectCounts "pipelines" config.pipelines
          with' tr_ "border-b" do
            with' td_ "p-1" "Vertices"
            with' td_ "p-1" (toHtml $ show $ Set.size $ Set.filter vForTenants analysis.vertices)

    with' div_ "pt-3" do
      pipelinesInfoComponent ctx analysis (Map.filterWithKey forTenants config.pipelines)
  where
    scope = case ctx.scope of
      Scoped tenants -> tenants
      UnScoped -> mempty
    otherTenants = Set.difference analysis.config.tenants scope
    config = analysis.config
    objectCounts :: Text -> Zuul.ConfigLoader.ConfigMap a b -> Html ()
    objectCounts n m = do
      with' tr_ "border-b" do
        with' td_ "p-1" (toHtml n)
        with' td_ "p-1" (toHtml $ show $ Map.size $ Map.filterWithKey forTenants m)
    forTenants :: a -> [(ConfigLoc, b)] -> Bool
    forTenants _ xs = case ctx.scope of
      Scoped tenants -> any (keepTenants tenants . fst) xs
      UnScoped -> True
    vForTenants v = case ctx.scope of
      Scoped tenants -> v.tenants `Set.isSubsetOf` tenants
      UnScoped -> True

    keepTenants :: Set TenantName -> ConfigLoc -> Bool
    keepTenants tenants loc = loc.tenants `Set.isSubsetOf` tenants
