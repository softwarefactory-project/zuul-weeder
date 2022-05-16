{-# LANGUAGE QuasiQuotes #-}

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
import Zuul.ConfigLoader
import ZuulWeeder.Graph
import ZuulWeeder.Prelude

-- | The data.json for the d3 graph (see dists/graph.js)
toD3Graph :: ConfigGraph -> ZuulWeeder.UI.D3Graph
toD3Graph g =
  ZuulWeeder.UI.D3Graph
    { ZuulWeeder.UI.nodes = toNodes <$> Algebra.Graph.vertexList g,
      ZuulWeeder.UI.links = toLinks <$> Algebra.Graph.edgeList g
    }
  where
    toNodes :: Vertex -> ZuulWeeder.UI.D3Node
    toNodes (_, e) = ZuulWeeder.UI.D3Node (display e) $ vertexGroup e
    toLinks :: (Vertex, Vertex) -> ZuulWeeder.UI.D3Link
    toLinks ((_, a), (_, b)) = ZuulWeeder.UI.D3Link (display a) (display b)

vertexGroup :: ConfigVertex -> Int
vertexGroup x = case x of
  VJob _ -> 1
  VProjectPipeline _ -> 2
  VNodeset _ -> 3
  VProjectTemplate _ -> 4
  VPipeline _ -> 5
  VNodeLabel _ -> 6
  VQueue _ -> 7
  VSemaphore _ -> 8

d3Color :: Int -> Text
d3Color x = case x of
  1 -> "#1f77b4"
  2 -> "#aec6e8"
  3 -> "#ff7f0e"
  4 -> "#ffbb78"
  5 -> "#2ca02c"
  6 -> "#98df8a"
  7 -> "#d62728"
  8 -> "#ff9896"
  _ -> "pink"

data D3Node = D3Node
  { name :: Text,
    group :: Int
  }
  deriving (Generic, Show)

data D3Link = D3Link
  { source :: Text,
    target :: Text
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

vertexLink :: Int -> ConfigName -> ConfigLoc -> ConfigVertex -> Html ()
vertexLink pos (ConfigName name) _loc vertex = with a_ [href_ ref] (vertexName vertex)
  where
    ref =
      Text.intercalate
        "/"
        [ "/object",
          via @VertexType vertex,
          name,
          Text.pack (show pos)
        ]

-- | Return the search result
searchResults :: Text -> Names -> Html ()
searchResults (Text.strip -> query) names
  | Text.null query = pure ()
  | otherwise = case filter matchQuery (Map.toList names) of
      [] -> div_ "no results :("
      results -> ul_ do
        forM_ results $ \(name, vertexes) ->
          forM_ (zip [0 ..] vertexes) $ \(pos, (loc, vertex)) ->
            with' li_ "bg-white/75" $ vertexLink pos name loc vertex
  where
    matchQuery (ConfigName name, _) = query `Text.isInfixOf` name

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

index :: Text -> Html () -> Html ()
index page mainComponent =
  doctypehtml_ do
    head_ do
      title_ "Zuul Weeder"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      link_ [href_ "/dists/tailwind.css", rel_ "stylesheet"]
    body_ do
      navComponent
      with div_ [class_ "container grid p-4", id_ "main"] mainComponent
      with (script_ mempty) [src_ "https://unpkg.com/htmx.org@1.7.0/dist/htmx.min.js"]
  where
    navComponent =
      with' nav_ "bg-slate-700 p-1 shadow w-full flex" do
        with' div_ "flex-grow" do
          with' span_ "font-semibold text-white" do
            with a_ [href_ "/"] "Zuul Weeder"
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
       in with a_ [href_ path, class_ linkClass]

infoComponent :: Config -> Html ()
infoComponent config = do
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
  with (script_ mempty) [src_ "https://d3js.org/d3.v4.min.js"]
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

vertexComponent :: Vertex -> Html ()
vertexComponent v@(_, cv) =
  dl (vertexSource v) $ vertexAttributes cv
  where
    vertexAttributes (ZuulConfigVertex _) = []
    vertexAttributes (NodeLabelVertex _) = []

vertexSource :: Vertex -> Html ()
vertexSource (loc, v) = span_ do
  vertexName v
  locComponent loc

vertexIcon :: ConfigVertex -> Html ()
vertexIcon cv = with span_ [style_ ("color: " <> (d3Color . vertexGroup $ cv))] $
  toHtml @Text $ case cv of
    VJob _ -> "⚙"
    VProjectPipeline _ -> "🎛"
    _ -> "X"

vertexName :: ConfigVertex -> Html ()
vertexName cv = span_ do
  vertexIcon cv
  toHtml n
  where
    ConfigName n = from cv

objectInfo :: ConfigName -> Int -> Analysis -> Html ()
objectInfo cn pos analysis = case Map.lookup cn analysis.names of
  Just (safeGet pos -> Just v) -> do
    h2_ $ vertexComponent v
    with' div_ "grid grid-cols-2 gap-1 m-4" do
      div_ do
        h3_ "Depends-On"
        renderVertexes (dependsOn v)
      div_ do
        h3_ "Requires"
        renderVertexes (required v)
  _ -> h2_ "Unknown object?!"
  where
    dependsOn v = Set.toList $ findReachable v analysis.configDependsOnGraph
    required v = Set.toList $ findReachable v analysis.configRequireGraph
    renderVertexes :: [Vertex] -> Html ()
    renderVertexes xs = do
      ul_ do
        traverse_ renderVertex xs
    renderVertex (loc, v) =
      li_ do
        vertexLink idx (from v) loc v
        with a_ [href_ (locUrl loc)] do
          with' span_ "px-1" "🔗"
      where
        idx :: Int
        idx = case Map.lookup (from v) analysis.names of
          Just xs -> fromMaybe -1 $ (loc, v) `elemIndex` xs
          Nothing -> -1

newtype VertexTypeUrl = VTU VertexType

instance FromHttpApiData VertexTypeUrl where
  parseUrlPiece txt = case txt of
    "job" -> pure . VTU . ZuulConfigVertexType $ JobT
    _ -> Left $ "Unknown obj type: " <> txt

newtype ConfigNameUrl = CNU ConfigName

instance FromHttpApiData ConfigNameUrl where
  parseUrlPiece = pure . CNU . ConfigName

type ObjectPath =
  "object" :> Capture "type" VertexTypeUrl
    :> Capture "name" ConfigNameUrl
    :> Capture "index" Int
    :> Get '[HTML] (Html ())

type API =
  Get '[HTML] (Html ())
    :<|> "search" :> Get '[HTML] (Html ())
    :<|> "info" :> Get '[HTML] (Html ())
    :<|> "about" :> Get '[HTML] (Html ())
    :<|> "search_results" :> ReqBody '[FormUrlEncoded] SearchForm :> Post '[HTML] (Html ())
    :<|> ObjectPath
    :<|> "data.json" :> Get '[JSON] D3Graph
    :<|> "dists" :> Raw

run :: IO Analysis -> IO ()
run config = do
  hPutStrLn stderr $ "[+] serving at " <> show port
  Warp.run port app
  where
    port = 8080
    app = serve (Proxy @API) server

    server :: Server API
    server =
      pure (index "/" welcomeComponent)
        :<|> pure (index "/search" searchComponent)
        :<|> infoRoute
        :<|> pure (index "/about" aboutComponent)
        :<|> searchRoute
        :<|> objectRoute
        :<|> d3Route
        :<|> staticRoute

    staticRoute = Servant.Server.StaticFiles.serveDirectoryWebApp "dists"

    infoRoute = do
      analysis <- liftIO config
      pure (index "/info" (infoComponent analysis.config))

    objectRoute (VTU _objType) (CNU configName) pos = do
      analysis <- liftIO config
      pure (index "/object" (objectInfo configName pos analysis))

    searchRoute req = do
      analysis <- liftIO config
      pure (searchResults req.query analysis.names)

    d3Route = do
      analysis <- liftIO config
      let graph = configRequireGraph analysis
      pure (toD3Graph graph)
