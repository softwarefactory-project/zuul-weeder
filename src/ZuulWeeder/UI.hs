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
module ZuulWeeder.UI
  ( app,
    BasePath (..),

    -- * Test helpers
    configLocUrl,
  )
where

-- After adding css class, run `nix run .#tailwind` to update the tailwind.css file. Then hard refresh the web page.

import Algebra.Graph qualified
import Data.Aeson qualified
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Lucid
import Lucid.Base (makeAttribute)
import Network.URI.Encode qualified
import Paths_zuul_weeder (version)
import Servant hiding (Context)
import Servant.HTML.Lucid (HTML)
import Servant.Server.StaticFiles qualified
import WaiAppStatic.Storage.Filesystem qualified
import WaiAppStatic.Types qualified
import Web.FormUrlEncoded (FromForm)
import Zuul.Config
import Zuul.ConfigLoader (Config (..), ConfigMap)
import ZuulWeeder.Graph
import ZuulWeeder.Prelude

-- | The request context
data Scope = UnScoped | Scoped (Set TenantName) deriving (Show)

-- | The base path of the web interface, when served behing a sub path proxy.
newtype BasePath = BasePath
  { basePath :: Text
  }
  deriving newtype (Show)

data Context = Context
  { rootURL :: BasePath,
    scope :: Scope
  }
  deriving (Show)

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
      link_ [href_ $ distUrl ctx "tailwind.css", rel_ "stylesheet"]
      with (script_ mempty) [src_ $ distUrl ctx "d3.v4.min.js"]
      with (script_ mempty) [src_ $ distUrl ctx "graph.js"]
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
|]
        <> cssColors

navComponent :: Context -> Text -> Html ()
navComponent ctx page =
  with' nav_ "bg-slate-700 p-1 shadow w-full flex" do
    with' div_ "flex-grow" do
      with' span_ "font-semibold text-white" do
        hxNavLink base Nothing "Zuul Weeder"
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
       in hxNavLinkWithAttr [id_ $ "nav-" <> path] (base <> path) (Just linkClass)
    exitScope =
      case ctx.scope of
        Scoped tenants -> hxNavLink (basePath ctx.rootURL) (Just tenantClass) (toHtml $ tenantsList tenants)
        UnScoped -> pure ()
      where
        tenantClass = "my-4 p-1 text-white font-semibold "

data VertexType
  = VAbstractJobT
  | VJobT
  | VNodesetT
  | VNodeLabelT
  | VProjectT
  | VProjectTemplateT
  | VPipelineT
  | VProjectPipelineT
  | VTemplatePipelineT
  | VTriggerT
  | VReporterT
  deriving (Enum, Bounded)

instance From VertexName VertexType where
  from = \case
    VAbstractJob _ -> VAbstractJobT
    VJob _ -> VJobT
    VProject _ -> VProjectT
    VNodeset _ -> VNodesetT
    VProjectTemplate _ -> VProjectTemplateT
    VPipeline _ -> VPipelineT
    VNodeLabel _ -> VNodeLabelT
    VProjectPipeline _ _ -> VProjectPipelineT
    VTemplatePipeline _ _ -> VTemplatePipelineT
    VTrigger _ -> VTriggerT
    VReporter _ -> VReporterT

vertexColor :: VertexType -> Text
vertexColor vt = "hsl(" <> from (show hue) <> ", 50%, 50%)"
  where
    vt' = case vt of
      VTemplatePipelineT -> VProjectTemplateT
      VProjectPipelineT -> VProjectT
      _ -> vt
    hue, step :: Int
    step = 300 `div` fromEnum @VertexType maxBound
    hue = fromEnum vt' * step

cssColors :: Text
cssColors = Text.unlines $ map mkCssColor [minBound .. maxBound]
  where
    mkCssColor :: VertexType -> Text
    mkCssColor vt =
      "." <> vertexTypeName vt <> " { color: " <> vertexColor vt <> ";}"

jsColors :: Text
jsColors =
  "const getColor = (group) => { switch (group) {\n" <> Text.unlines (map mkJsColor [minBound .. maxBound]) <> "}};"
  where
    mkJsColor :: VertexType -> Text
    mkJsColor vt = "case " <> from (show (fromEnum vt)) <> ": return \"" <> vertexColor vt <> "\";"

-- | A text representation of a vertex type, useful for /object url piece.
vertexTypeName :: VertexType -> Text
vertexTypeName = \case
  VAbstractJobT -> "abstract-job"
  VJobT -> "job"
  VNodesetT -> "nodeset"
  VNodeLabelT -> "label"
  VProjectT -> "project"
  VProjectTemplateT -> "project-template"
  VPipelineT -> "pipeline"
  VProjectPipelineT -> "project-pipeline"
  VTemplatePipelineT -> "template-pipeline"
  VTriggerT -> "trigger"
  VReporterT -> "reporter"

spinner :: Html ()
spinner = with span_ [class_ "htmx-indicator font-semibold text-white", id_ "spinner"] "◌"

hxNavLinkWithAttr :: [Attribute] -> Text -> Maybe Text -> Html () -> Html ()
hxNavLinkWithAttr xs url extraClass =
  with
    a_
    ( xs
        <> [ hxGet url,
             hxPushUrl,
             hxIndicator "#spinner",
             hxTarget "#main",
             class_ ("cursor-pointer hover:font-semibold" <> maybe "" (mappend " ") extraClass),
             href_ url
           ]
    )

hxNavLink :: Text -> Maybe Text -> Html () -> Html ()
hxNavLink = hxNavLinkWithAttr []

welcomeComponent :: Context -> Html ()
welcomeComponent ctx = do
  searchComponent ctx Nothing mempty
  script_ do
    "renderToy('" <> baseUrl ctx <> "data.json');"

title :: Text -> Html ()
title = with' h2_ "font-bold" . toHtml

mkIconClass :: Maybe Text -> Text -> Html ()
mkIconClass cl name = with' i_ ("pr-1 font-bold align-bottom " <> name <> maybe "" (mappend " ") cl) mempty

mkIcon :: Text -> Html ()
mkIcon = mkIconClass Nothing

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
      vertexTypeIcon vt
      toHtml $ vertexTypeName vt

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

-- | Get the URL of a configuration element location
configLocUrl :: ConfigLoc -> Text
configLocUrl loc = case loc.url of
  GerritUrl url -> trimedUrl url <> "/plugins/gitiles/" <> name <> "/+/refs/heads/" <> branch <> "/" <> path
  GithubUrl url -> buildGithubUrl url
  GitlabUrl url -> buildGitlabUrl url
  PagureUrl url -> buildPagureUrl url
  GitUrl url
    | "gitlab.com" `Text.isInfixOf` url -> buildGitlabUrl url
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

-- | The data.json for the d3 graph (see dists/graph.js)
toD3Graph :: Scope -> ConfigGraph -> ZuulWeeder.UI.D3Graph
toD3Graph scope g =
  ZuulWeeder.UI.D3Graph
    { ZuulWeeder.UI.nodes = toNodes <$> vertexes,
      ZuulWeeder.UI.links = toLinks <$> edges
    }
  where
    -- Keep the edges whose both vertex are in the current tenant
    keepTenant (a, b) = case scope of
      Scoped tenants -> tenants == a.tenants && tenants == b.tenants
      UnScoped -> True

    (edges, _) = splitAt 500 $ filter keepTenant $ Algebra.Graph.edgeList g
    -- edges = Algebra.Graph.edgeList g
    vertexes = nub $ concatMap (\(a, b) -> [a, b]) edges

    toNodes :: Vertex -> ZuulWeeder.UI.D3Node
    toNodes v = ZuulWeeder.UI.D3Node (from v.name) (hash v) $ fromEnum (into @VertexType v.name)

    toLinks :: (Vertex, Vertex) -> ZuulWeeder.UI.D3Link
    toLinks (a, b) = ZuulWeeder.UI.D3Link (hash a) (hash b)

vertexTypeIcon :: VertexType -> Html ()
vertexTypeIcon vt = mkIconClass (Just $ vertexTypeName vt) ("ri-" <> iconName)
  where
    iconName = case vt of
      VAbstractJobT -> "file-text-line"
      VJobT -> "file-text-line"
      VProjectT -> "folder-open-line"
      VProjectTemplateT -> "draft-line"
      VPipelineT -> "git-merge-line"
      VNodeLabelT -> "price-tag-3-line"
      VProjectPipelineT -> "git-merge-line"
      VTemplatePipelineT -> "git-merge-line"
      VNodesetT -> "server-line"
      VTriggerT -> "download-fill"
      VReporterT -> "upload-fill"

data D3Node = D3Node
  { name :: Text,
    id :: Int,
    group :: Int
  }
  deriving (Generic, Eq, Show)

data D3Link = D3Link
  { source :: Int,
    target :: Int
  }
  deriving (Generic, Show)

data D3Graph = D3Graph
  { nodes :: [D3Node],
    links :: [D3Link]
  }
  deriving (Generic, Show)

instance Data.Aeson.ToJSON D3Node

instance Data.Aeson.ToJSON D3Link

instance Data.Aeson.ToJSON D3Graph

vertexLink :: Context -> VertexName -> Html () -> Html ()
vertexLink ctx name = hxNavLink ref Nothing
  where
    ref =
      Text.intercalate
        "/"
        [ baseUrl ctx <> "object",
          vertexTypeName (from name),
          -- TODO: url encode name
          Network.URI.Encode.encodeText (from name)
        ]

tenantBaseLink :: BasePath -> TenantName -> Html ()
tenantBaseLink rootURL tenant =
  with' span_ "ml-2 px-1 bg-slate-300 rounded" do
    hxNavLink (tenantUrl rootURL tenant) Nothing (toHtml (into @Text tenant))

tenantLink :: BasePath -> VertexName -> TenantName -> Html ()
tenantLink rootURL name tenant =
  with' span_ "ml-2 px-1 bg-slate-300 rounded" do
    vertexLink (Context rootURL (Scoped $ Set.singleton tenant)) name (toHtml (into @Text tenant))

vertexName :: VertexName -> Html ()
vertexName n = do
  vertexTypeIcon (from n)
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

with' :: With a => a -> Text -> a
with' x n = with x [class_ n]

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

hxTrigger, hxTarget, hxGet, hxPost, hxIndicator :: Text -> Attribute
hxTrigger = makeAttribute "hx-trigger"
hxTarget = makeAttribute "hx-target"
hxGet = makeAttribute "hx-get"
hxPost = makeAttribute "hx-post"
hxIndicator = makeAttribute "hx-indicator"

hxPushUrl :: Attribute
hxPushUrl = makeAttribute "hx-push-url" "true"

infoComponent :: Context -> Analysis -> Html ()
infoComponent ctx analysis = do
  with' div_ "grid p-4 place-content-center" do
    with' span_ "font-semibold pb-3" do
      "Config details"
      traverse_ (tenantBaseLink ctx.rootURL) scope
    with' div_ "pb-3" do
      unless (Set.null otherTenants) $ do
        "Available tenants:"
        traverse_ (tenantBaseLink ctx.rootURL) otherTenants
    with' div_ "not-prose bg-slate-50 border rounded-xl w-80" do
      with' table_ "table-auto border-collapse w-80" do
        thead_ $ with' tr_ "border-b text-left" $ traverse_ (with' th_ "p-1") ["Object", "Count"]
        with' tbody_ "bg-white" do
          objectCounts "jobs" config.jobs
          objectCounts "nodesets" config.nodesets
          objectCounts "pipelines" config.pipelines
  where
    scope = case ctx.scope of
      Scoped tenants -> tenants
      UnScoped -> mempty
    otherTenants = Set.difference analysis.tenants scope
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
    keepTenants :: Set TenantName -> ConfigLoc -> Bool
    keepTenants tenants loc = tenants `Set.isSubsetOf` loc.tenants

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

locLink :: ConfigLoc -> Html ()
locLink loc =
  with a_ [href_ url, class_ "no-underline hover:text-slate-500 p-1 text-slate-700"] do
    mkIcon "ri-link"
    toHtml locPath
  where
    -- TODO: render valid link based on config connection
    url = configLocUrl loc
    locPath = Text.drop 8 url

objectInfo :: Context -> NonEmpty Vertex -> Analysis -> Html ()
objectInfo ctx vertices analysis = do
  h2_ do
    vertexTypeIcon (from vertex.name)
    toHtml (from vertex.name :: Text)
  ul_ do
    traverse_ renderConfigLink configComponents
  with' div_ "grid grid-cols-2 gap-1 m-4" do
    div_ do
      title "Dependents"
      traverse_ (renderTree 0) dependents
    div_ do
      title "Dependencies"
      traverse_ (renderTree 0) dependencies
  where
    renderConfigLink loc =
      li_ do
        locLink loc
        traverse_ (tenantLink ctx.rootURL vertex.name) loc.tenants

    vertex = NE.head vertices
    forTenant :: ConfigLoc -> Bool
    forTenant loc = case ctx.scope of
      Scoped tenants -> tenants `Set.isSubsetOf` loc.tenants
      UnScoped -> True

    getLocs :: Maybe [(ConfigLoc, a)] -> [ConfigLoc]
    getLocs = filter forTenant . maybe [] (fmap fst)
    configComponents :: [ConfigLoc]
    configComponents = case vertex.name of
      VAbstractJob name -> getLocs $ Map.lookup name analysis.config.jobs
      VJob name -> getLocs $ Map.lookup name analysis.config.jobs
      VProject name -> getLocs $ Map.lookup name analysis.config.projects
      VProjectTemplate name -> getLocs $ Map.lookup name analysis.config.projectTemplates
      VPipeline name -> getLocs $ Map.lookup name analysis.config.pipelines
      VNodeset name -> getLocs $ Map.lookup name analysis.config.nodesets
      VNodeLabel name -> getLocs $ Map.lookup name analysis.config.nodeLabels
      VProjectPipeline name _ -> getLocs $ Map.lookup name analysis.config.projects
      VTemplatePipeline name _ -> getLocs $ Map.lookup name analysis.config.projectTemplates
      VTrigger name -> getLocs $ Map.lookup name analysis.config.triggers
      VReporter name -> getLocs $ Map.lookup name analysis.config.reporters
    dependencies = getForest analysis.dependencyGraph
    dependents = getForest analysis.dependentGraph
    getForest = ZuulWeeder.Graph.findReachableForest tenantsM vertices
      where
        tenantsM = case ctx.scope of
          UnScoped -> Nothing
          Scoped xs -> Just xs

    renderTree :: Int -> Tree VertexName -> Html ()
    renderTree depth (Node root childs) = do
      let listStyle
            | depth > 0 = "pl-2 border-solid rounded border-l-2 border-slate-500"
            | otherwise = ""
      with' ul_ listStyle do
        li_ $ vertexLink ctx root (vertexName root)
        traverse_ (renderTree (depth + 1)) childs

vertexScope :: Scope -> Set Vertex -> [Vertex]
vertexScope scope vertices = Set.toList $ case scope of
  UnScoped -> vertices
  Scoped tenants -> Set.filter (matchTenant tenants) vertices
  where
    matchTenant tenants v = tenants `Set.isSubsetOf` v.tenants

newtype VertexTypeUrl = VTU (Text -> VertexName)

instance FromHttpApiData VertexTypeUrl where
  parseUrlPiece txt = pure . VTU $ case txt of
    "abstract-job" -> VAbstractJob . JobName
    "job" -> VJob . JobName
    "nodeset" -> VNodeset . NodesetName
    "label" -> VNodeLabel . NodeLabelName
    "project" -> VProject . ProjectName
    "project-template" -> VProjectTemplate . ProjectTemplateName
    "pipeline" -> VPipeline . PipelineName
    "project-pipeline" -> brk VProjectPipeline ProjectName
    "template-pipeline" -> brk VTemplatePipeline ProjectTemplateName
    "trigger" -> VTrigger . ConnectionName
    "reporter" -> VReporter . ConnectionName
    _ -> error $ "Unknown obj type: " <> from txt
    where
      brk vType nType t =
        let (a, Text.tail -> b) = Text.span (/= ':') t
         in vType (nType a) (PipelineName b)

newtype VertexNameUrl = CNU Text

instance FromHttpApiData VertexNameUrl where
  parseUrlPiece = pure . CNU

newtype TenantsUrl = TNU {getTNU :: Set TenantName}

instance FromHttpApiData TenantsUrl where
  parseUrlPiece piece = pure . TNU $ Set.fromList (TenantName <$> Text.split (== ',') piece)

type GetRequest = Header "HX-Request" Text :> Get '[HTML] (Html ())

-- | The zuul-weeder base HTTP API.
-- The HX-Request header is set by inline navigation, when it is missing, the full body is returned.
type BaseAPI =
  GetRequest
    :<|> "about" :> GetRequest
    :<|> "search" :> GetRequest
    :<|> "info" :> GetRequest
    :<|> "debug" :> GetRequest
    :<|> "object" :> Capture "type" VertexTypeUrl :> Capture "name" VertexNameUrl :> GetRequest
    :<|> "search_results" :> SearchPath
    :<|> "search" :> Capture "query" Text :> Get '[HTML] (Html ())
    :<|> "data.json" :> Get '[JSON] D3Graph

type SearchPath =
  ReqBody '[FormUrlEncoded] SearchForm
    :> Post '[HTML] (Headers '[Header "HX-Push" Text] (Html ()))

type TenantAPI = "tenant" :> Capture "tenant" TenantsUrl :> BaseAPI

type StaticAPI = "dists" :> Raw

type API = StaticAPI :<|> BaseAPI :<|> TenantAPI

-- | Creates the Web Application Interface (wai).
app ::
  -- | An action to refresh the analysis
  IO Analysis ->
  -- | The base path of the interface, used to render absolute links
  BasePath ->
  -- | The location of the static files
  FilePath ->
  -- | The application to serve
  Application
app config rootURL distPath = serve (Proxy @API) rootServer
  where
    rootServer :: Server API
    rootServer =
      Servant.Server.StaticFiles.serveDirectoryWith staticSettings
        :<|> server (Context rootURL UnScoped)
        :<|> server . Context rootURL . Scoped . getTNU

    staticSettings =
      (WaiAppStatic.Storage.Filesystem.defaultWebAppSettings distPath)
        { WaiAppStatic.Types.ssMaxAge = WaiAppStatic.Types.NoMaxAge
        }

    server :: Context -> Server BaseAPI
    server ctx =
      indexRoute "" (pure $ welcomeComponent ctx)
        :<|> indexRoute "about" (pure aboutComponent)
        :<|> flip searchRoute Nothing
        :<|> indexRoute "info" (infoComponent ctx <$> liftIO config)
        :<|> indexRoute "debug" (debugComponent <$> liftIO config)
        :<|> objectRoute
        :<|> searchResultRoute
        :<|> searchRouteWithQuery
        :<|> d3Route
      where
        indexRoute :: Text -> Handler (Html ()) -> Maybe a -> Handler (Html ())
        -- The HX-Request header is missing, return the full body
        indexRoute name component Nothing = mainBody ctx name <$> component
        -- The HX-Request header is set, return the component and update the nav links
        indexRoute name component (Just _htmxRequest) = do
          componentHtml <- component
          pure do
            navComponent ctx name
            with div_ [class_ "container grid p-4"] componentHtml

        objectRoute (VTU mkName) (CNU name) htmxRequest = do
          analysis <- liftIO config
          let vname = mkName name
          let vertices = vertexScope ctx.scope $ Set.filter matchVertex analysis.vertices
                where
                  matchVertex v = v.name == vname
          let component = case NE.nonEmpty vertices of
                Just xs -> pure (objectInfo ctx xs analysis)
                Nothing -> pure "not found!"
          indexRoute "object" component htmxRequest

        -- /search/query does not come from htmx, the body is always served
        searchRouteWithQuery query = searchRoute Nothing (Just query)

        searchRoute htmxRequest queryM = do
          result <- case queryM of
            Just query -> do
              analysis <- liftIO config
              pure . snd $ searchResults ctx query analysis.names
            Nothing -> pure mempty
          indexRoute "search" (pure $ searchComponent ctx queryM result) htmxRequest

        searchResultRoute req = do
          analysis <- liftIO config
          let (value, result) = searchResults ctx req.query analysis.names
          pure $ addHeader (maybe "false" (mappend (baseUrl ctx <> "search/")) value) result

        d3Route = do
          analysis <- liftIO config
          let graph = dependencyGraph analysis
          pure (toD3Graph ctx.scope graph)
