-- |
-- Module      : ZuulWeeder.UI.D3
-- Description : A graph representation suitable for D3
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
module ZuulWeeder.UI.D3 (D3Graph, toD3Graph) where

import Algebra.Graph qualified
import Data.Aeson qualified
import ZuulWeeder.Graph
import ZuulWeeder.Prelude
import ZuulWeeder.UI

-- | The data.json for the d3 graph (see dists/graph.js)
toD3Graph :: Scope -> ConfigGraph -> D3Graph
toD3Graph scope g =
  D3Graph
    { nodes = toNodes <$> vertexes,
      links = toLinks <$> edges
    }
  where
    -- Keep the edges whose both vertex are in the current tenant
    keepTenant (a, b) = case scope of
      Scoped tenants -> tenants == a.tenants && tenants == b.tenants
      UnScoped -> True

    (edges, _) = splitAt 500 $ filter keepTenant $ Algebra.Graph.edgeList g
    -- edges = Algebra.Graph.edgeList g
    vertexes = nub $ concatMap (\(a, b) -> [a, b]) edges

    toNodes :: Vertex -> D3Node
    toNodes v = D3Node (from v.name) (hash v) $ fromEnum (into @VertexType v.name)

    toLinks :: (Vertex, Vertex) -> D3Link
    toLinks (a, b) = D3Link (hash a) (hash b)

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
