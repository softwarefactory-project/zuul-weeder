module Zuul.Weeder (main) where

import Control.Monad.State (execStateT)
import Streaming
import qualified Streaming.Prelude as S
import System.Environment
import qualified Zuul.ConfigLoader
import Zuul.ZKDump

main :: IO ()
main = do
  [path] <- getArgs
  config <- loadConfig path
  putStrLn $ "Got: " <> show config

loadConfig :: FilePath -> IO Zuul.ConfigLoader.Config
loadConfig =
  flip execStateT Zuul.ConfigLoader.emptyConfig
    -- StateT Config IO ()
    . S.effects
    -- Apply the loadConfig function to each element
    . S.chain Zuul.ConfigLoader.loadConfig
    -- Stream (Of ZKConfig) (StateT Config IO)
    . hoist lift
    -- Stream (Of ZKConfig) IO
    . walkConfigNodes
