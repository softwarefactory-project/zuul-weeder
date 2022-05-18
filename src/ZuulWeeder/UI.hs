-- | The web interface for zuul-weeder
-- The UI is implemented with
-- * htmx - https://htmx.org/docs/#introduction
-- * tailwind - https://tailwindcss.com/docs/utility-first  (use Ctrl-K to search documentation)
--
-- After adding css class, run `nix run .#tailwind` to update the tailwind.css file. Then hard refresh the web page.
module ZuulWeeder.UI where

import Algebra.Graph qualified
import Data.Aeson qualified
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String.QQ (s)
import Data.Text qualified as Text
import Lucid
import Lucid.Base (makeAttribute)
import Network.Wai.Handler.Warp as Warp (run)
import Servant hiding (Context)
import Servant.HTML.Lucid (HTML)
import Servant.Server.StaticFiles qualified
import Web.FormUrlEncoded (FromForm)
import Zuul.Config
import Zuul.ConfigLoader (Config (..), ConfigMap)
import ZuulWeeder.Graph
import ZuulWeeder.Prelude

configLocUrl :: ConfigLoc -> Text
configLocUrl loc = case loc.url of
  GerritUrl url ->
    Text.dropWhileEnd (== '/') url <> "/plugins/gitiles/" <> name <> "/+/refs/heads/" <> branch <> "/" <> path
  GitUrl _ -> error "TODO"
  where
    CanonicalProjectName (_, ProjectName name) = loc.project
    BranchName branch = loc.branch
    FilePathT path = loc.path

-- | The request context
type Context = Maybe (Set TenantName)

-- | The data.json for the d3 graph (see dists/graph.js)
toD3Graph :: Context -> ConfigGraph -> ZuulWeeder.UI.D3Graph
toD3Graph tenantsM g =
  ZuulWeeder.UI.D3Graph
    { ZuulWeeder.UI.nodes = toNodes <$> vertexes,
      ZuulWeeder.UI.links = toLinks <$> edges
    }
  where
    -- Keep the edges whose both vertex are in the current tenant
    keepTenant (a, b) = case tenantsM of
      Just tenants -> tenants `Set.isSubsetOf` a.tenants && tenants `Set.isSubsetOf` b.tenants
      Nothing -> True

    (edges, _) = splitAt 500 $ filter keepTenant $ Algebra.Graph.edgeList g
    -- edges = Algebra.Graph.edgeList g
    vertexes = nub $ concatMap (\(a, b) -> [a, b]) edges
    toNodes :: Vertex -> ZuulWeeder.UI.D3Node
    toNodes v = ZuulWeeder.UI.D3Node (toNode v) (nodeID v) $ vertexGroup v.name

    toNode :: Vertex -> Text
    toNode v = from v.name

    toLinks :: (Vertex, Vertex) -> ZuulWeeder.UI.D3Link
    toLinks (a, b) = ZuulWeeder.UI.D3Link (nodeID a) (nodeID b)

    nodeID :: Vertex -> Int
    nodeID = hash

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
vertexLink tenantsM name = with a_ [href_ ref]
  where
    ref =
      Text.intercalate
        "/"
        [ tenantsSlug tenantsM <> "/object",
          vertexTypeName name,
          -- TODO: url encode name
          from name
        ]

vertexName :: VertexName -> Html ()
vertexName n = do
  vertexTypeIcon n
  toHtml (into @Text n)

-- | Return the search result
searchResults :: Context -> Text -> Map VertexName (Set TenantName) -> Html ()
searchResults tenantsM (Text.strip -> query) names
  | Text.null query = pure ()
  | otherwise = case mapMaybe matchQuery (Map.toList names) of
      [] -> div_ "no results :("
      results -> ul_ do
        forM_ results $ \(name, tenants) ->
          with' li_ "bg-white/75" $ do
            vertexLink tenantsM name (vertexName name)
            case tenantsM of
              Just _ -> pure ()
              Nothing ->
                forM_ tenants $ \tenant ->
                  with' span_ "ml-2 px-1 bg-slate-300 rounded" do
                    vertexLink (Just $ Set.singleton tenant) name (toHtml (into @Text tenant))
  where
    matchTenant vertexTenants = case tenantsM of
      -- The provided context match the vertex, keep the context
      Just tenants | tenants `Set.isSubsetOf` vertexTenants -> Just tenants
      -- The provided context does not match
      Just _ -> Nothing
      -- No context was provided, keep the vertex tenants
      Nothing -> Just vertexTenants
    matchQuery :: (VertexName, Set TenantName) -> Maybe (VertexName, Set TenantName)
    matchQuery (name, tenants) = case (query `Text.isInfixOf` from name, matchTenant tenants) of
      (True, Just matchingTenants) -> Just (name, matchingTenants)
      _ -> Nothing

newtype SearchForm = SearchForm {query :: Text} deriving (Eq, Show, Generic)

instance FromForm SearchForm

with' :: With a => a -> Text -> a
with' x n = with x [class_ n]

searchComponent :: Context -> Html ()
searchComponent tenants = do
  with' div_ "grid p-4 place-content-center" do
    input_
      [ class_ "form-control",
        size_ "42",
        type_ "search",
        name_ "query",
        placeholder_ "Begin Typing To Search Config...",
        hxPost (tenantsSlug tenants <> "/search_results"),
        hxTrigger "keyup changed delay:500ms, search",
        hxTarget "#search-results"
      ]
    with div_ [id_ "search-results"] mempty

hxTrigger, hxTarget, hxSwap, hxGet, hxPost, hxBoost :: Text -> Attribute
hxTrigger = makeAttribute "hx-trigger"
hxTarget = makeAttribute "hx-target"
hxSwap = makeAttribute "hx-swap"
hxGet = makeAttribute "hx-get"
hxPost = makeAttribute "hx-post"
hxBoost = makeAttribute "hx-boost"

tenantsList :: Set TenantName -> Text
tenantsList tenants = Text.intercalate "," (from <$> Set.toList tenants)

tenantsSlug :: Context -> Text
tenantsSlug = \case
  Nothing -> ""
  Just tenants -> "tenant/" <> tenantsList tenants <> "/"

index :: Context -> Text -> Html () -> Html ()
index tenantsM page mainComponent =
  doctypehtml_ do
    head_ do
      title_ "Zuul Weeder"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      link_ [href_ "/dists/tailwind.css", rel_ "stylesheet"]
    body_ do
      navComponent
      with div_ [class_ "container grid p-4", id_ "main"] mainComponent
      with (script_ mempty) [src_ "/dists/htmx.min.js"]
  where
    rootUrl = "/"
    baseUrl = rootUrl <> tenantsSlug tenantsM

    navComponent =
      with' nav_ "bg-slate-700 p-1 shadow w-full flex" do
        with' div_ "flex-grow" do
          with' span_ "font-semibold text-white" do
            with a_ [href_ baseUrl] "Zuul Weeder"
          navLink "search" "Search"
          navLink "info" "Info"
        div_ do
          case tenantsM of
            Just tenants -> with a_ [href_ rootUrl, tenantClass] (toHtml $ tenantsList tenants)
            Nothing -> pure ()
          navLink "about" "About"
    tenantClass = class_ "my-4 p-1 text-white font-semibold "
    navLink path =
      let navLinkClass
            | "/" <> path == page || (path == "search" && page == "/") = " bg-slate-500"
            | otherwise = ""
          extra
            | path == "/about" = " right"
            | otherwise = ""
          linkClass = "m-4 p-1 text-white rounded hover:text-teal-500" <> navLinkClass <> extra
       in with a_ [href_ (baseUrl <> path), class_ linkClass]

infoComponent :: Context -> Config -> Html ()
infoComponent tenantsM config = do
  h2_ "Config details"
  with' div_ "grid p-4 place-content-center" do
    with' div_ "not-prose bg-slate-50 border rounded-xl" do
      with' table_ "table-auto border-collapse w-80" do
        thead_ $ with' tr_ "border-b text-left" $ traverse_ (with' th_ "p-1") ["Object", "Count"]
        with' tbody_ "bg-white" do
          objectCounts "jobs" config.jobs
          objectCounts "nodesets" config.nodesets
          objectCounts "pipelines" config.pipelines
  where
    objectCounts :: Text -> Zuul.ConfigLoader.ConfigMap a b -> Html ()
    objectCounts n m = do
      with' tr_ "border-b" do
        with' td_ "p-1" (toHtml n)
        with' td_ "p-1" (toHtml $ show $ Map.size $ Map.filterWithKey forTenants m)
    forTenants :: a -> [(ConfigLoc, b)] -> Bool
    forTenants _ xs = case tenantsM of
      Just tenants -> any (keepTenants tenants . fst) xs
      Nothing -> True
    keepTenants :: Set TenantName -> ConfigLoc -> Bool
    keepTenants tenants loc = tenants `Set.isSubsetOf` loc.tenants

aboutComponent :: Html ()
aboutComponent = do
  h2_ "Welcome"
  p_ "Zuul Weeder is a web service to inspect Zuul configuration"

welcomeComponent :: Context -> Html ()
welcomeComponent tenants = do
  searchComponent tenants
  style_ css
  with (script_ mempty) [src_ "/dists/d3.v4.min.js"]
  with (script_ mempty) [src_ "/dists/graph.js"]
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

locUrl :: ConfigLoc -> Text
locUrl loc = "https://" <> locPath
  where
    CanonicalProjectName (ProviderName providerName, ProjectName projectName) = loc.project
    locPath = Text.intercalate "/" [providerName, projectName, getPath loc.path]

locLink :: ConfigLoc -> Html ()
locLink loc =
  with a_ [href_ url, class_ "no-underline hover:text-slate-500 p-1 text-slate-700"] do
    with' span_ "px-1" "🔗"
    toHtml locPath
  where
    -- TODO: render valid link based on config connection
    url = locUrl loc
    locPath = Text.drop 8 url

locComponent :: ConfigLoc -> Html ()
locComponent loc = do
  locLink loc
  traverse_ tenantBox loc.tenants
  where
    tenantBox (TenantName tenant) = with' span_ "rounded bg-slate-200 text-slate-700 p-1" (toHtml tenant)

dl :: Html () -> [(Text, Html ())] -> Html ()
dl title xs =
  with' div_ "bg-white shadow overflow-hidden sm:rounded-lg" do
    with' div_ "px-4 py-5 sm:px-6" do
      with' h3_ "text-lg leading-6 font-medium text-gray-900" title
    with' div_ "border-t border-gray-200" do
      dl_ (traverse_ go (zip [0 ..] xs))
  where
    go :: (Int, (Text, Html ())) -> Html ()
    go (pos, (k, v)) =
      with' div_ (bgColor <> " px-1 py-2 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6") do
        with' dt_ "text-sm font-medium text-gray-500" (toHtml k)
        with' dd_ "mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2" v
      where
        bgColor
          | even pos = "bg-gray-50"
          | otherwise = "bg-white"

objectInfo :: Context -> NonEmpty Vertex -> Analysis -> Html ()
objectInfo tenantsM vertices analysis = do
  h2_ do
    vertexTypeIcon vertex.name
    toHtml (from vertex.name :: Text)
  ul_ do
    traverse_ (li_ . locLink) configComponents
  with' div_ "grid grid-cols-2 gap-1 m-4" do
    div_ do
      h3_ "Depends-On"
      renderVertexes dependsOn
    div_ do
      h3_ "Requires"
      renderVertexes required
  where
    vertex = NE.head vertices
    forTenant :: ConfigLoc -> Bool
    forTenant loc = case tenantsM of
      Just tenants -> tenants `Set.isSubsetOf` loc.tenants
      Nothing -> True

    getLocs :: Maybe [(ConfigLoc, a)] -> [ConfigLoc]
    getLocs = filter forTenant . maybe [] (fmap fst)
    configComponents :: [ConfigLoc]
    configComponents = case vertex.name of
      VJob name -> getLocs $ Map.lookup name analysis.config.jobs
      VProject name -> getLocs $ Map.lookup name analysis.config.projects
      VProjectTemplate name -> getLocs $ Map.lookup name analysis.config.projectTemplates
      VPipeline name -> getLocs $ Map.lookup name analysis.config.pipelines
      _ -> error "Config lookup not implemented"
    dependsOn = Set.toList $ findReachable vertices analysis.configDependsOnGraph
    required = Set.toList $ findReachable vertices analysis.configRequireGraph
    renderVertexes :: [Vertex] -> Html ()
    renderVertexes xs = do
      ul_ do
        traverse_ renderVertex xs
    renderVertex v =
      li_ do
        vertexLink tenantsM v.name (vertexName v.name)

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

type ObjectPath =
  Capture "type" VertexTypeUrl
    :> Capture "name" VertexNameUrl
    :> Get '[HTML] (Html ())

type BaseAPI =
  Get '[HTML] (Html ())
    :<|> "about" :> Get '[HTML] (Html ())
    :<|> "search" :> Get '[HTML] (Html ())
    :<|> "info" :> Get '[HTML] (Html ())
    :<|> "search_results" :> ReqBody '[FormUrlEncoded] SearchForm :> Post '[HTML] (Html ())
    :<|> "object" :> ObjectPath
    :<|> "data.json" :> Get '[JSON] D3Graph

type TenantAPI = "tenant" :> Capture "tenant" TenantsUrl :> BaseAPI

type StaticAPI = "dists" :> Raw

type API = StaticAPI :<|> BaseAPI :<|> TenantAPI

run :: IO Analysis -> IO ()
run config = do
  hPutStrLn stderr $ "[+] serving at " <> show port
  Warp.run port app
  where
    port = 8080
    app = serve (Proxy @API) rootServer

    rootServer :: Server API
    rootServer =
      Servant.Server.StaticFiles.serveDirectoryWebApp "dists"
        :<|> server Nothing
        :<|> server . Just . getTNU

    server :: Context -> Server BaseAPI
    server tenants =
      pure (index tenants "/" (welcomeComponent tenants))
        :<|> pure (index tenants "/about" aboutComponent)
        :<|> pure (index tenants "/search" (searchComponent tenants))
        :<|> infoRoute
        :<|> searchRoute
        :<|> objectRoute
        :<|> d3Route
      where
        infoRoute = do
          analysis <- liftIO config
          pure (index tenants "/info" (infoComponent tenants analysis.config))

        objectRoute (VTU mkName) (CNU name) = do
          analysis <- liftIO config
          let vname = mkName name
          let vertices = Set.toList $ Set.filter matchVertex analysis.vertices
                where
                  matchVertex v = v.name == vname && matchTenant v
                  matchTenant v = case tenants of
                    Just xs -> xs `Set.isSubsetOf` v.tenants
                    Nothing -> True
          case NE.nonEmpty vertices of
            Just xs -> pure (index tenants "/object" (objectInfo tenants xs analysis))
            Nothing -> pure "not found!"

        searchRoute req = do
          analysis <- liftIO config
          pure (searchResults tenants req.query analysis.names)

        d3Route = do
          analysis <- liftIO config
          let graph = configRequireGraph analysis
          pure (toD3Graph tenants graph)
