-- |
-- Module      : ZuulWeeder.UI.Colors
-- Description : The vertex to color automatic convertion
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
module ZuulWeeder.UI.Colors (vertexHue, jsColors, cssColors) where

import Data.Text qualified as Text
import ZuulWeeder.Graph
import ZuulWeeder.Prelude
import ZuulWeeder.UI.Vertex

-- | This value is used for dot export
vertexHue :: VertexType -> Int
vertexHue vt = fromEnum vt' * step
 where
  vt' = case vt of
    VTemplatePipelineT -> VProjectTemplateT
    VProjectPipelineT -> VProjectT
    _ -> vt
  step :: Int
  step = 300 `div` fromEnum @VertexType maxBound

vertexColor :: VertexType -> Text
vertexColor vt = "hsl(" <> from (show $ vertexHue vt) <> ", 50%, 50%)"

cssColors :: Text
cssColors = Text.unlines $ map mkCssColor [minBound .. maxBound]
 where
  mkCssColor :: VertexType -> Text
  mkCssColor vt =
    ".color-" <> vertexSlugName vt <> " { color: " <> vertexColor vt <> ";}"

jsColors :: Text
jsColors =
  "const getColor = (group) => { switch (group) {\n" <> Text.unlines (map mkJsColor [minBound .. maxBound]) <> "}};"
 where
  mkJsColor :: VertexType -> Text
  mkJsColor vt = "case " <> from (show (fromEnum vt)) <> ": return \"" <> vertexColor vt <> "\";"
