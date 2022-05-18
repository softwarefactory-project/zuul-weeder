-- | The web interface for zuul-weeder
-- The UI is implemented with
-- * htmx - https://htmx.org/docs/#introduction
-- * tailwind - https://tailwindcss.com/docs/utility-first  (use Ctrl-K to search documentation)
--
-- After adding css class, run `nix run .#tailwind` to update the tailwind.css file. Then hard refresh the web page.
module ZuulWeeder.UI where

import Algebra.Graph qualified
import Data.Aeson qualified
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String.QQ (s)
import Data.Text qualified as Text
import Lucid
import Lucid.Base (makeAttribute)
import Network.Wai.Handler.Warp as Warp (run)
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Server.StaticFiles qualified
import Web.FormUrlEncoded (FromForm)
import Zuul.Config
import Zuul.ConfigLoader (Config (..))
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

-- | The data.json for the d3 graph (see dists/graph.js)
toD3Graph :: Maybe TenantName -> ConfigGraph -> ZuulWeeder.UI.D3Graph
toD3Graph tenant g =
  ZuulWeeder.UI.D3Graph
    { ZuulWeeder.UI.nodes = toNodes <$> vertexes,
      ZuulWeeder.UI.links = toLinks <$> edges
    }
  where
    -- Keep the edges whose both vertex are in the current tenant
    keepTenant (a, b) = case tenant of
      Just t -> t `elem` a.tenants && t `elem` b.tenants
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

vertexLink :: Vertex -> Html ()
vertexLink v = with a_ [href_ ref] $ do
  vertexTypeIcon v.name
  toHtml name
  where
    name = from v.name
    ref =
      Text.intercalate
        "/"
        [ "/object",
          vertexTypeName v.name,
          -- TODO: url encode name
          name,
          -- TODO: base64 url encode tenant list
          Text.intercalate "," (getName <$> v.tenants)
        ]

-- | Return the search result
searchResults :: Maybe TenantName -> Text -> Set Vertex -> Html ()
searchResults tenant (Text.strip -> query) vertices
  | Text.null query = pure ()
  | otherwise = case filter matchQuery (Set.toList vertices) of
      [] -> div_ "no results :("
      results -> ul_ do
        forM_ results $ \vertex ->
          -- TODO: group jobs that have the same name, but different in tenant, and add a badge to differentiate name
          with' li_ "bg-white/75" $ vertexLink vertex
  where
    matchTenant = case tenant of
      Just tenant' -> \v -> tenant' `elem` v.tenants
      Nothing -> const True
    matchQuery v = query `Text.isInfixOf` from v.name && matchTenant v

mkTooltip :: Text -> Html ()
mkTooltip n =
  with div_ [id_ "tooltip-default", role_ "tooltip", class_ "inline-block absolute invisible z-10 py-2 px-3 text-sm font-medium text-white bg-gray-900 rounded-lg shadow-sm opacity-0 transition-opacity duration-300 tooltip dark:bg-gray-700"] do
    toHtml n
    with div_ [class_ "tooltip-arrow", makeAttribute "data-popper-arrow" mempty] mempty

newtype SearchForm = SearchForm {query :: Text} deriving (Eq, Show, Generic)

instance FromForm SearchForm

with' :: With a => a -> Text -> a
with' x n = with x [class_ n]

searchComponent :: Html ()
searchComponent = do
  with' div_ "grid p-4 place-content-center" do
    input_
      [ class_ "form-control",
        size_ "42",
        type_ "search",
        name_ "query",
        placeholder_ "Begin Typing To Search Config...",
        hxPost "/search_results",
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

index :: Maybe TenantName -> Text -> Html () -> Html ()
index tenantName page mainComponent =
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
    baseUrl = case tenantName of
      Just (TenantName name) -> "/tenant/" <> name
      Nothing -> ""

    navComponent =
      with' nav_ "bg-slate-700 p-1 shadow w-full flex" do
        with' div_ "flex-grow" do
          with' span_ "font-semibold text-white" do
            with a_ [href_ (baseUrl <> "/")] "Zuul Weeder"
          navLink "/search" "Search"
          navLink "/info" "Info"
        div_ do
          navLink "/about" "About"
    navLink path =
      let navLinkClass
            | path == page || (path == "/search" && page == "/") = " bg-slate-500"
            | otherwise = ""
          extra
            | path == "/about" = " right"
            | otherwise = ""
          linkClass = "m-4 p-1 text-white rounded hover:text-teal-500" <> navLinkClass <> extra
       in with a_ [href_ (baseUrl <> path), class_ linkClass]

infoComponent :: Maybe TenantName -> Config -> Html ()
infoComponent _tenant config = do
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
    objectCounts :: Text -> Map a b -> Html ()
    objectCounts n m = do
      -- TODO: filter tenant
      with' tr_ "border-b" do
        with' td_ "p-1" (toHtml n)
        with' td_ "p-1" (toHtml $ show $ Map.size m)

aboutComponent :: Html ()
aboutComponent = do
  h2_ "Welcome"
  p_ "Zuul Weeder is a web service to inspect Zuul configuration"

welcomeComponent :: Html ()
welcomeComponent = do
  searchComponent
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

objectInfo :: Maybe TenantName -> Vertex -> Analysis -> Html ()
objectInfo tenant v analysis = do
  h2_ do
    vertexTypeIcon v.name
    toHtml (from v.name :: Text)
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
    tenants = case tenant of
      Just x -> [x]
      Nothing -> v.tenants
    forTenant :: ConfigLoc -> Bool
    forTenant loc = any (`elem` loc.tenants) tenants
    getLocs :: Maybe [(ConfigLoc, a)] -> [ConfigLoc]
    getLocs = filter forTenant . maybe [] (fmap fst)
    configComponents :: [ConfigLoc]
    configComponents = case v.name of
      VJob name -> getLocs $ Map.lookup name analysis.config.jobs
      VProject name -> getLocs $ Map.lookup name analysis.config.projects
      VProjectTemplate name -> getLocs $ Map.lookup name analysis.config.projectTemplates
      VPipeline name -> getLocs $ Map.lookup name analysis.config.pipelines
      _ -> error "Config lookup not implemented"
    dependsOn = Set.toList $ findReachable v analysis.configDependsOnGraph
    required = Set.toList $ findReachable v analysis.configRequireGraph
    renderVertexes :: [Vertex] -> Html ()
    renderVertexes xs = do
      ul_ do
        traverse_ renderVertex xs
    renderVertex v' =
      li_ do
        vertexLink v'

newtype VertexTypeUrl = VTU (Text -> VertexName)

instance FromHttpApiData VertexTypeUrl where
  parseUrlPiece txt = case txt of
    "job" -> pure . VTU $ VJob . JobName
    "nodeset" -> pure . VTU $ VNodeset . NodesetName
    "label" -> pure $ VTU $ VNodeLabel . NodeLabelName
    "project" -> pure $ VTU $ VProject . ProjectName
    "project-template" -> pure $ VTU $ VProjectTemplate . ProjectTemplateName
    "pipeline" -> pure $ VTU $ VPipeline . PipelineName
    _ -> Left $ "Unknown obj type: " <> txt

newtype ConfigNameUrl = CNU Text

instance FromHttpApiData ConfigNameUrl where
  parseUrlPiece = pure . CNU

newtype TenantsUrl = TNU [TenantName]

instance FromHttpApiData TenantsUrl where
  parseUrlPiece piece = pure . TNU $ TenantName <$> Text.split (== ',') piece

type ObjectPath =
  Capture "type" VertexTypeUrl
    :> Capture "name" ConfigNameUrl
    :> Capture "tenants" TenantsUrl
    :> Get '[HTML] (Html ())

type BaseAPI =
  Get '[HTML] (Html ())
    :<|> "about" :> Get '[HTML] (Html ())
    :<|> "search" :> Get '[HTML] (Html ())
    :<|> "info" :> Get '[HTML] (Html ())
    :<|> "search_results" :> ReqBody '[FormUrlEncoded] SearchForm :> Post '[HTML] (Html ())
    :<|> "object" :> ObjectPath
    :<|> "data.json" :> Get '[JSON] D3Graph

type TenantAPI = "tenant" :> Capture "tenant" Text :> BaseAPI

type StaticAPI =
  "dists" :> Raw

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
        :<|> server . Just . TenantName

    server tenant =
      pure (index tenant "/" welcomeComponent)
        :<|> pure (index tenant "/about" aboutComponent)
        :<|> pure (index tenant "/search" searchComponent)
        :<|> infoRoute tenant
        :<|> searchRoute tenant
        :<|> objectRoute tenant
        :<|> d3Route tenant

    infoRoute tenant = do
      analysis <- liftIO config
      pure (index tenant "/info" (infoComponent tenant analysis.config))

    objectRoute tenant (VTU mkName) (CNU name) (TNU tenants) = do
      analysis <- liftIO config
      pure (index tenant "/object" (objectInfo tenant (Vertex tenants (mkName name)) analysis))

    searchRoute tenant req = do
      analysis <- liftIO config
      pure (searchResults tenant req.query analysis.vertices)

    d3Route tenant = do
      analysis <- liftIO config
      let graph = configRequireGraph analysis
      pure (toD3Graph tenant graph)
