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
import Servant hiding (Context)
import Servant.HTML.Lucid (HTML)
import Servant.Server.StaticFiles qualified
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
      link_ [href_ $ distUrl ctx "tailwind.css", rel_ "stylesheet"]
      with (script_ mempty) [src_ $ distUrl ctx "d3.v4.min.js"]
      with (script_ mempty) [src_ $ distUrl ctx "graph.js"]
      with (script_ mempty) [src_ $ distUrl ctx "htmx.min.js"]
      style_ css
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
             class_ ("cursor-pointer" <> maybe "" (mappend " ") extraClass),
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

aboutComponent :: Html ()
aboutComponent = do
  h2_ "Welcome"
  p_ "Zuul Weeder is a web service to inspect Zuul configuration"

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
    buildGitlabUrl url = trimedUrl url <> name <> "/-/blob/" <> branch <> "/" <> path
    buildPagureUrl url = trimedUrl url <> name <> "/blob/" <> branch <> "/f/" <> path
    buildGithubUrl url = trimedUrl url <> name <> "/blob/" <> branch <> "/" <> path

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
    toNodes v = ZuulWeeder.UI.D3Node (from v.name) (hash v) $ vertexGroup v.name

    toLinks :: (Vertex, Vertex) -> ZuulWeeder.UI.D3Link
    toLinks (a, b) = ZuulWeeder.UI.D3Link (hash a) (hash b)

vertexGroup :: VertexName -> Int
vertexGroup = \case
  VJob _ -> 1
  VProject _ -> 2
  VNodeset _ -> 3
  VProjectTemplate _ -> 4
  VPipeline _ -> 5
  VNodeLabel _ -> 6

-- Keep in sync with graph.js getColor function
d3Color :: VertexName -> Text
d3Color = \case
  VJob _ -> "#1f77b4"
  VProject _ -> "#aec6e8"
  VNodeset _ -> "#ff7f0e"
  VProjectTemplate _ -> "#ffbb78"
  VPipeline _ -> "#2ca02c"
  VNodeLabel _ -> "#98df8a"

vertexTypeIcon :: VertexName -> Html ()
vertexTypeIcon vn = with span_ [style_ ("color: " <> d3Color vn)] $
  toHtml @Text $ case vn of
    VJob _ -> "⚙"
    VProject _ -> "P"
    VNodeset _ -> "N"
    VProjectTemplate _ -> "🎛"
    VPipeline _ -> "P"
    VNodeLabel _ -> "L"

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
          vertexTypeName name,
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
  vertexTypeIcon n
  toHtml (into @Text n)

-- | Return the search result
searchResults :: Context -> Text -> Map VertexName (Set TenantName) -> (Maybe Text, Html ())
searchResults ctx (Text.strip -> query) names
  | Text.null query = (Nothing, pure ())
  | otherwise = case mapMaybe matchQuery (Map.toList names) of
      [] -> (Nothing, div_ "no results :(")
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
    with' div_ "not-prose bg-slate-50 border rounded-xl" do
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

locLink :: ConfigLoc -> Html ()
locLink loc =
  with a_ [href_ url, class_ "no-underline hover:text-slate-500 p-1 text-slate-700"] do
    with' span_ "px-1" "🔗"
    toHtml locPath
  where
    -- TODO: render valid link based on config connection
    url = configLocUrl loc
    locPath = Text.drop 8 url

objectInfo :: Context -> NonEmpty Vertex -> Analysis -> Html ()
objectInfo ctx vertices analysis = do
  h2_ do
    vertexTypeIcon vertex.name
    toHtml (from vertex.name :: Text)
  ul_ do
    traverse_ renderConfigLink configComponents
  with' div_ "grid grid-cols-2 gap-1 m-4" do
    div_ do
      h3_ "Depends-On"
      renderVertexes dependsOn
    div_ do
      h3_ "Requires"
      renderVertexes required
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
      VJob name -> getLocs $ Map.lookup name analysis.config.jobs
      VProject name -> getLocs $ Map.lookup name analysis.config.projects
      VProjectTemplate name -> getLocs $ Map.lookup name analysis.config.projectTemplates
      VPipeline name -> getLocs $ Map.lookup name analysis.config.pipelines
      VNodeset name -> getLocs $ Map.lookup name analysis.config.nodesets
      VNodeLabel name -> getLocs $ Map.lookup name analysis.config.nodeLabels
    dependsOn = Set.toList $ findReachable vertices analysis.configDependsOnGraph
    required = Set.toList $ findReachable vertices analysis.configRequireGraph
    renderVertexes :: [Vertex] -> Html ()
    renderVertexes xs = do
      ul_ do
        traverse_ renderVertex xs
    renderVertex v =
      li_ do
        vertexLink ctx v.name (vertexName v.name)

newtype VertexTypeUrl = VTU (Text -> VertexName)

instance FromHttpApiData VertexTypeUrl where
  parseUrlPiece txt = pure . VTU $ case txt of
    "job" -> VJob . JobName
    "nodeset" -> VNodeset . NodesetName
    "label" -> VNodeLabel . NodeLabelName
    "project" -> VProject . ProjectName
    "project-template" -> VProjectTemplate . ProjectTemplateName
    "pipeline" -> VPipeline . PipelineName
    _ -> error $ "Unknown obj type: " <> from txt

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
      Servant.Server.StaticFiles.serveDirectoryWebApp distPath
        :<|> server (Context rootURL UnScoped)
        :<|> server . Context rootURL . Scoped . getTNU
    server :: Context -> Server BaseAPI
    server ctx =
      indexRoute "" (pure $ welcomeComponent ctx)
        :<|> indexRoute "about" (pure aboutComponent)
        :<|> flip searchRoute Nothing
        :<|> indexRoute "info" (infoComponent ctx <$> liftIO config)
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
            componentHtml

        objectRoute (VTU mkName) (CNU name) htmxRequest = do
          analysis <- liftIO config
          let vname = mkName name
          let vertices = Set.toList $ Set.filter matchVertex analysis.vertices
                where
                  matchVertex v = v.name == vname && matchTenant v
                  matchTenant v = case ctx.scope of
                    Scoped xs -> xs `Set.isSubsetOf` v.tenants
                    UnScoped -> True
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
          let graph = configRequireGraph analysis
          pure (toD3Graph ctx.scope graph)
