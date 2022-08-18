-- TODO: remove warning when refactor is complet
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- |
-- Module      : ZuulWeeder.UI
-- Description : The User Interface
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
--
-- Maintainer  : tdecacqu@redhat.com, fboucher@redhat.com
-- Stability   : provisional
-- Portability : portable
--
-- The web interface for zuul-weeder.
--
-- The UI is implemented with
--
--   * [htmx](https://htmx.org/docs/#introduction)
--   * [tailwind](https://tailwindcss.com/docs/utility-first) (use Ctrl-K to search documentation)
module ZuulWeeder.UI where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Lucid
import Paths_zuul_weeder (version)
import Web.FormUrlEncoded (FromForm)
import Zuul.Config
import Zuul.ConfigLoader (Config (..))
import ZuulWeeder.Graph
import ZuulWeeder.Prelude
import ZuulWeeder.UI.CSS
import ZuulWeeder.UI.Colors
import ZuulWeeder.UI.Vertex

-- | The request context
data Scope = UnScoped | Scoped (Set TenantName) deriving (Ord, Eq, Show)

-- | The base path of the web interface, when served behing a sub path proxy.
newtype BasePath = BasePath
  { basePath :: Text
  }
  deriving newtype (Ord, Eq, Show)

data Context = Context
  { rootURL :: BasePath,
    scope :: Scope
  }
  deriving (Ord, Eq, Show)

mainBody :: Context -> Text -> Html () -> Html ()
mainBody ctx page mainComponent =
  doctypehtml_ do
    head_ do
      title_ "Zuul Weeder"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      script_ jsColors
      style_ css
      link_ [href_ $ distUrl ctx "remixicon.min.css", rel_ "stylesheet"]
      link_ [href_ $ distUrl ctx ("tailwind.css?version=" <> gitVersion), rel_ "stylesheet"]
      with (script_ mempty) [src_ $ distUrl ctx "d3.v4.min.js"]
      with (script_ mempty) [src_ $ distUrl ctx ("graph.js?version=" <> gitVersion)]
      with (script_ mempty) [src_ $ distUrl ctx "htmx.min.js"]
    with body_ [id_ "main"] do
      navComponent ctx page
      with div_ [class_ "container grid p-4"] mainComponent
  where
    css :: Text
    css =
      [s|
.links line {
  stroke: #999;
  stroke-opacity: 0.6;
}
svg#d3 {
  position: fixed;
  height: 100%;
  width: 100%;
  margin: 0;
  top: 0;
  left: 0;
  z-index: -1;
}

.tree-caret {
  cursor: pointer;
  user-select: none;
  font-size: 10px;
  vertical-align: super;
  color: rgb(100, 116, 139);
}

.tree-caret::before {
  content: "\25B6";
  display: inline-block;
  color: rgb(100, 116, 139);
  margin-right: 6px;
}

.tree-caret-down::before {
  transform: rotate(90deg);
}

.nested {
  display: none;
}

.active {
  display: block;
}
|]
        <> cssColors

navComponent :: Context -> Text -> Html ()
navComponent ctx page =
  with' nav_ "bg-slate-700 p-1 shadow w-full flex" do
    with' div_ "flex-grow" do
      with' span_ "font-semibold text-white" do
        hxNavLink [] base Nothing "Zuul Weeder"
      navLink "search" "Search"
      navLink "info" "Info"
    div_ do
      exitScope
      navLink "about" "About"
      spinner
  where
    base = baseUrl ctx
    navLink path =
      let navLinkClass
            | path == page || (path == "search" && page == "") = " bg-slate-500"
            | otherwise = ""
          extra
            | path == "about" = " right"
            | otherwise = ""
          linkClass = "m-4 p-1 cursor-pointer text-white rounded hover:text-teal-500" <> navLinkClass <> extra
       in hxNavLink [id_ $ "nav-" <> path] (base <> path) (Just linkClass)
    exitScope =
      case ctx.scope of
        Scoped tenants -> hxNavLink [] (basePath ctx.rootURL) (Just tenantClass) (toHtml $ tenantsList tenants)
        UnScoped -> pure ()
      where
        tenantClass = "my-4 p-1 text-white font-semibold "

welcomeComponent :: Context -> Html ()
welcomeComponent ctx = do
  searchComponent ctx Nothing mempty
  script_ do
    "renderToy('" <> baseUrl ctx <> "data.json');"

titleWithTooltip :: Text -> Text -> Html ()
titleWithTooltip tooltip value = with h2_ [class_ "font-bold", title_ tooltip] (toHtml value)

aboutComponent :: Html ()
aboutComponent = do
  title "Welcome"
  with' p_ "pb-5" "Zuul Weeder is a web service to inspect Zuul configuration"
  title "Icons"
  with' ul_ "pb-5" do
    traverse_ renderIconLegend [minBound .. maxBound]
  div_ . toHtml $ "Version: " <> from (showVersion version) <> " (" <> gitVersion <> ")"
  where
    renderIconLegend :: VertexType -> Html ()
    renderIconLegend vt = li_ do
      vertexIcon vt
      toHtml $ vertexSlugName vt

tenantsList :: Set TenantName -> Text
tenantsList tenants = Text.intercalate "," (from <$> Set.toList tenants)

distUrl :: Context -> Text -> Text
distUrl ctx x = basePath ctx.rootURL <> "dists/" <> x

tenantUrl :: BasePath -> TenantName -> Text
tenantUrl (BasePath rootURL) (TenantName name) = rootURL <> "tenant/" <> name <> "/"

baseUrl :: Context -> Text
baseUrl ctx =
  basePath ctx.rootURL <> case ctx.scope of
    UnScoped -> ""
    Scoped tenants -> "tenant/" <> tenantsList tenants <> "/"

vertexLink :: Context -> VertexName -> Html () -> Html ()
vertexLink ctx name = hxNavLink [] ref Nothing
  where
    ref =
      Text.intercalate
        "/"
        [ baseUrl ctx <> "object",
          vertexSlugName (from name),
          from name
        ]

tenantBaseLink :: BasePath -> TenantName -> Html ()
tenantBaseLink rootURL tenant =
  with' span_ "ml-2 px-1 bg-slate-300 rounded" do
    hxNavLink [] (tenantUrl rootURL tenant) Nothing (toHtml (into @Text tenant))

tenantInfoLink :: BasePath -> TenantName -> Html ()
tenantInfoLink rootURL tenant =
  with' span_ "ml-2 px-1 bg-slate-300 rounded" do
    hxNavLink [] (tenantUrl rootURL tenant <> "info") Nothing (toHtml (into @Text tenant))

tenantLink :: BasePath -> VertexName -> TenantName -> Html ()
tenantLink rootURL name tenant =
  with' span_ "ml-2 px-1 bg-slate-300 rounded" do
    vertexLink (Context rootURL (Scoped $ Set.singleton tenant)) name (toHtml (into @Text tenant))

vertexName :: VertexName -> Html ()
vertexName n = do
  vertexIcon (from n)
  toHtml (into @Text n)

-- | Return the search result
searchResults :: Context -> Text -> Map VertexName (Set TenantName) -> (Maybe Text, Html ())
searchResults ctx (Text.strip -> query) names
  | Text.null query = (Nothing, pure ())
  | otherwise = case mapMaybe matchQuery (Map.toList names) of
      [] -> trace (from $ pShowNoColor names) (Nothing, div_ "no results :(")
      results -> (Just query, ul_ $ traverse_ renderResult results)
  where
    renderResult :: (VertexName, Set TenantName) -> Html ()
    renderResult (name, tenants) =
      with' li_ "bg-white/75" $ do
        vertexLink ctx name (vertexName name)
        case ctx.scope of
          -- When scoped, don't display tenant badge
          Scoped _ -> pure ()
          UnScoped -> traverse_ (tenantLink ctx.rootURL name) tenants

    matchTenant vertexTenants = case ctx.scope of
      -- The provided context match the vertex, keep the context
      Scoped tenants | tenants `Set.isSubsetOf` vertexTenants -> Just tenants
      -- The provided context does not match
      Scoped _ -> Nothing
      -- No context was provided, keep the vertex tenants
      UnScoped -> Just vertexTenants
    matchQuery :: (VertexName, Set TenantName) -> Maybe (VertexName, Set TenantName)
    matchQuery (name, tenants) = case (query `Text.isInfixOf` from name, matchTenant tenants) of
      (True, Just matchingTenants) -> Just (name, matchingTenants)
      _ -> Nothing

newtype SearchForm = SearchForm {query :: Text} deriving (Eq, Show, Generic)

instance FromForm SearchForm

searchComponent :: Context -> Maybe Text -> Html () -> Html ()
searchComponent ctx queryM result = do
  with' div_ "grid p-4 place-content-center" do
    input_
      [ class_ "form-control",
        size_ "42",
        type_ "search",
        name_ "query",
        hxPost (baseUrl ctx <> "search_results"),
        hxTrigger "keyup changed delay:500ms, search",
        hxTarget "#search-results",
        attr
      ]
    with div_ [id_ "search-results"] result
  where
    attr = case queryM of
      Just q -> value_ q
      Nothing -> placeholder_ "Begin Typing To Search Config..."

isJobVertex :: VertexName -> Bool
isJobVertex = \case
  VJob {} -> True
  _ -> False

isPipelineConfig :: VertexName -> Maybe PipelineName
isPipelineConfig = \case
  VProjectPipeline n _ -> Just n
  VTemplatePipeline n _ -> Just n
  VRegexPipeline n _ -> Just n
  _ -> Nothing

debugComponent :: Analysis -> Html ()
debugComponent analysis = do
  with' h2_ "font-bold" "Debug Info"
  unless (null analysis.config.configErrors) do
    with' div_ "font-semibold py-3" "Config Error"
    ul_ do
      traverse_ (li_ . toHtml . take 512 . show) analysis.config.configErrors
  unless (null analysis.graphErrors) do
    with' div_ "font-semibold py-3" "Graph Error"
    ul_ do
      traverse_ (li_ . toHtml . take 512) analysis.graphErrors

locLink :: Text -> Html ()
locLink url =
  with a_ [href_ url, class_ "no-underline hover:text-slate-500 p-1 text-slate-700"] do
    mkIcon Nothing "ri-link"
    toHtml locPath
  where
    locPath = Text.drop 8 url

vertexScope :: Scope -> Set Vertex -> [Vertex]
vertexScope scope vertices = Set.toList $ case scope of
  UnScoped -> vertices
  Scoped tenants -> Set.filter (matchTenant tenants) vertices
  where
    matchTenant tenants v = tenants `Set.isSubsetOf` v.tenants
