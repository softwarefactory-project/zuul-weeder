{-# LANGUAGE TemplateHaskell #-}

-- | The web interface for zuul-weeder
module Zuul.UI where

import Data.Aeson qualified
import Data.FileEmbed (embedStringFile)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lucid
import Network.Wai.Handler.Warp as Warp (run)
import Servant
import Servant.HTML.Lucid (HTML)

type API =
  Get '[HTML] (Html ())
    :<|> "data.json" :> Get '[JSON] D3Graph

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
      style_ $(embedStringFile "./src/Zuul/UI.css")
    body_ do
      with div_ [id_ "header"] "Welcome"
      with svg_ [width_ "1000", height_ "800"] mempty
      with (script_ mempty) [src_ "https://d3js.org/d3.v4.min.js"]
      script_ $(embedStringFile "./src/Zuul/UI.js")

d3Graph :: D3Graph
d3Graph =
  D3Graph
    { nodes =
        [ D3Node "check" 0,
          D3Node "gate" 0,
          D3Node "base" 1,
          D3Node "linters" 1,
          D3Node "centos" 2
        ],
      links =
        [ D3Link "check" "base",
          D3Link "gate" "base",
          D3Link "check" "linters",
          D3Link "base" "centos",
          D3Link "linters" "centos"
        ]
    }

run :: D3Graph -> IO ()
run g = Warp.run port app
  where
    port = 8080
    app = serve (Proxy @API) server

    server :: Server API
    server = pure index :<|> pure g
