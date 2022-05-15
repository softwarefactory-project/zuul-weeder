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
    toNodes (_, e) = ZuulWeeder.UI.D3Node (display e) $ case e of
      VJob _ -> 1
      VProjectPipeline _ -> 2
      VNodeset _ -> 3
      VProjectTemplate _ -> 4
      VPipeline _ -> 5
      VNodeLabel _ -> 6
      VQueue _ -> 7
      VSemaphore _ -> 8
    toLinks :: (Vertex, Vertex) -> ZuulWeeder.UI.D3Link
    toLinks ((_, a), (_, b)) = ZuulWeeder.UI.D3Link (display a) (display b)

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

-- | Return the search result
searchResults :: Text -> Names -> Html ()
searchResults (Text.strip -> query) names
  | Text.null query = pure ()
  | otherwise = case filter matchQuery (Map.toList names) of
      [] -> div_ "no results :("
      results -> ul_ do
        forM_ results $ \result ->
          li_ $ toHtml (show result)
  where
    matchQuery (ConfigName name, _) = query `Text.isInfixOf` name

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
      with div_ [class_ "container", id_ "main"] mainComponent
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

infoComponent :: Html ()
infoComponent = do
  h2_ "Config details"

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
svg {
  position: fixed;
  height: 100%;
  width: 100%;
  margin: 0;
  top: 0;
  z-index: -1;
}
|]

type API =
  Get '[HTML] (Html ())
    :<|> "search" :> Get '[HTML] (Html ())
    :<|> "info" :> Get '[HTML] (Html ())
    :<|> "about" :> Get '[HTML] (Html ())
    :<|> "search_results" :> ReqBody '[FormUrlEncoded] SearchForm :> Post '[HTML] (Html ())
    :<|> "data.json" :> Get '[JSON] D3Graph
    :<|> "dists" :> Raw

run :: IO Analysis -> IO ()
run config = Warp.run port app
  where
    port = 8080
    app = serve (Proxy @API) server

    server :: Server API
    server =
      pure (index "/" welcomeComponent)
        :<|> pure (index "/search" searchComponent)
        :<|> pure (index "/info" infoComponent)
        :<|> pure (index "/about" aboutComponent)
        :<|> searchRoute
        :<|> d3Route
        :<|> staticRoute

    staticRoute = Servant.Server.StaticFiles.serveDirectoryWebApp "dists"

    searchRoute req = do
      analysis <- liftIO config
      pure (searchResults req.query analysis.names)

    d3Route = do
      analysis <- liftIO config
      let graph = configRequireGraph analysis
      pure (toD3Graph graph)
