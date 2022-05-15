{-# LANGUAGE QuasiQuotes #-}

-- | The web interface for zuul-weeder
module ZuulWeeder.UI where

import Data.Aeson qualified
import Data.String.QQ (s)
import Lucid
import Network.Wai.Handler.Warp as Warp (run)
import Servant
import Servant.HTML.Lucid (HTML)
import ZuulWeeder.Prelude
import ZuulWeeder.Graph
import qualified Algebra.Graph

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

index :: Html ()
index =
  doctypehtml_ do
    head_ do
      title_ "Zuul Weeder"
      meta_ [charset_ "utf-8"]
      style_ css
    body_ do
      with div_ [id_ "header"] "Welcome"
      with svg_ [width_ "1000", height_ "800"] mempty
      with (script_ mempty) [src_ "https://d3js.org/d3.v4.min.js"]
      script_ d3Script

css :: Text
css =
  [s|
.links line {
  stroke: #999;
  stroke-opacity: 0.6;
}
|]

d3Script :: Text
d3Script =
  [s|
// Based on https://bl.ocks.org/mbostock/4062045
var svg = d3.select("svg"),
  width = +svg.attr("width"),
  height = +svg.attr("height");

var color = d3.scaleOrdinal(d3.schemeCategory20);

var simulation = d3
  .forceSimulation()
  .force(
    "link",
    d3.forceLink().id((d) => d.name)
  )
  .force("charge", d3.forceManyBody().strength(-3))
  .force("center", d3.forceCenter(width / 2, height / 2));

d3.json("data.json", function (error, graph) {
  if (error) throw error;

  var link = svg
    .append("g")
    .attr("class", "links")
    .selectAll("line")
    .data(graph.links)
    .enter()
    .append("line")
    .attr("stroke-width", (d) => 1);

  var node = svg
    .append("g")
    .attr("class", "nodes")
    .selectAll("circle")
    .data(graph.nodes)
    .enter()
    .append("circle")
    .attr("r", 5)
    .attr("fill", (d) => color(d.group))
    .call(
      d3
        .drag()
        .on("start", dragstarted)
        .on("drag", dragged)
        .on("end", dragended)
    );

  node.append("title").text((d) => d.name);

  simulation.nodes(graph.nodes).on("tick", ticked);

  simulation.force("link").links(graph.links);

  function ticked() {
    link
      .attr("x1", (d) => d.source.x)
      .attr("y1", (d) => d.source.y)
      .attr("x2", (d) => d.target.x)
      .attr("y2", (d) => d.target.y);

    node.attr("cx", (d) => d.x).attr("cy", (d) => d.y);
  }
});

function dragstarted(d) {
  if (!d3.event.active) simulation.alphaTarget(0.3).restart();
  d.fx = d.x;
  d.fy = d.y;
}

function dragged(d) {
  d.fx = d3.event.x;
  d.fy = d3.event.y;
}

function dragended(d) {
  if (!d3.event.active) simulation.alphaTarget(0);
  d.fx = null;
  d.fy = null;
}
|]

type API =
  Get '[HTML] (Html ())
    :<|> "data.json" :> Get '[JSON] D3Graph

run :: IO Analysis -> IO ()
run config = Warp.run port app
  where
    port = 8080
    app = serve (Proxy @API) server

    server :: Server API
    server = pure index :<|> d3Route

    d3Route = do
      analysis <- liftIO config
      let graph = configRequireGraph analysis
      pure (toD3Graph graph)
