-- |
-- Module      : ZuulWeeder.UI
-- Description : A graph representation for dot
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
module ZuulWeeder.UI.Dot (dotGraph, dotLegend) where

import Algebra.Graph qualified
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import ZuulWeeder.Graph
import ZuulWeeder.Prelude
import ZuulWeeder.UI.Colors
import ZuulWeeder.UI.Vertex

-- | Render the analysis as a graphviz graph.
dotGraph :: Analysis -> Text
dotGraph analysis = "digraph G {" <> Text.unlines dotGraph' <> "}"
  where
    dotGraph' =
      (Text.unlines . nodeStyles <$> Map.toList allNodes)
        <> ["edge [color=\"gold2\"]"]
        <> dotEdges analysis.dependencyGraph
        <> ["edge [color=\"pink\"]"]
        <> dotEdges analysis.dependentGraph
    nodeStyles :: (VertexType, [Text]) -> [Text]
    nodeStyles (vt, objs) =
      mappend "   "
        <$> [ "node [" <> dotShape vt <> " " <> dotColor vt <> "];",
              Text.unwords (map (\v -> "\"" <> v <> "\"") objs) <> ";"
            ]
    allNodes :: Map VertexType [Text]
    allNodes =
      Map.fromListWith mappend $
        Set.toList $
          Set.map (\v -> (from v.name, [from v.name])) $
            analysis.vertices

    dotEdges :: ConfigGraph -> [Text]
    dotEdges = fmap dotEdge . Algebra.Graph.edgeList
    dotEdge :: (Vertex, Vertex) -> Text
    dotEdge (v1, v2) = "  \"" <> from v1.name <> "\" -> \"" <> from v2.name <> "\""

dotColor :: VertexType -> Text
dotColor vt = from msg
  where
    msg :: String
    msg = printf "color=\"%0.2f+0.5+0.5\"" (fromInteger (toInteger (vertexHue vt)) / 360.0 :: Float)

dotShape :: VertexType -> Text
dotShape vt = from msg
  where
    msg :: String
    msg = printf "shape=%s style=filled" $ case vt of
      VProjectT -> "box"
      VProjectTemplateT -> "box"
      VProjectRegexT -> "box"
      VPipelineT -> pipelineShape
      VProjectPipelineT -> pipelineShape
      VRegexPipelineT -> pipelineShape
      VTemplatePipelineT -> pipelineShape
      VReporterT -> "rarrow"
      VTriggerT -> "larrow"
      VJobT -> "ellipse"
      VAbstractJobT -> "ellipse"
      VNodeLabelT -> "tab"
      VNodesetT -> "component"
      _ -> "cylinder"
    pipelineShape :: String
    pipelineShape = "hexagon"

-- | A graphviz graph for the legend.
dotLegend :: Text
dotLegend = "digraph G {" <> Text.unlines dotGraph' <> "}"
  where
    dotGraph' =
      mappend " "
        <$> [ "rankdir=\"RL\";",
              "label = \"Legend\";",
              "shape = rectable;",
              "color = black;",
              "\"-dependent->\" [fontcolor=pink shape=plaintext fontsize=20 fontname=\"times bold\"];",
              "\"-dependency->\" [fontcolor=gold2 shape=plaintext fontsize=20 fontname=\"times bold\"];"
            ]
          <> map mkLegend [minBound .. maxBound]
    mkLegend :: VertexType -> Text
    mkLegend vt =
      Text.unwords
        [ "\"" <> vertexSlugName vt <> "\"",
          "[",
          dotShape vt,
          dotColor vt,
          "];"
        ]
