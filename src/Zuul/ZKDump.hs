module Zuul.ZKDump
  ( walk,
    mkZKConfig,
    ZKConfig (..),
  )
where

import Control.Monad (forM_, void, when)
import Control.Monad.Trans (liftIO)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
  ( Text,
    breakOn,
    drop,
    dropWhile,
    intercalate,
    isPrefixOf,
    isSuffixOf,
    pack,
    replace,
    split,
  )
import Network.URI.Encode (decodeText)
import qualified Streaming.Prelude as S
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

data ZKConfig = ZKConfig
  { provider :: T.Text,
    project :: T.Text,
    branch :: T.Text,
    filePath :: T.Text,
    zkData :: Maybe T.Text
  }
  deriving (Show, Eq)

walk :: FilePath -> S.Stream (S.Of ZKConfig) IO ()
walk dumpPath = walkRecursive $ dumpPath </> "zuul" </> "config" </> "cache"
  where
    walkRecursive :: FilePath -> S.Stream (S.Of ZKConfig) IO ()
    walkRecursive curPath = do
      tree <- liftIO $ listDirectory curPath
      forM_ tree $ \path -> do
        let fullPath = curPath </> path
        isDirectory <- liftIO $ doesDirectoryExist fullPath
        if isDirectory
          then walkRecursive fullPath
          else do
            when ("/0000000000/ZKDATA" `T.isSuffixOf` T.pack fullPath) $ do
              case mkZKConfig fullPath of
                Nothing -> pure ()
                Just zc -> S.yield zc

mkZKConfig :: FilePath -> Maybe ZKConfig
mkZKConfig path = do
  [_, _, filePath', _, branch', providerPath'] <-
    pure $
      take 6 $ reverse $ T.split (== '/') (T.pack path)
  (provider, project) <- case T.breakOn "/" $ decodeText providerPath' of
    (_, "") -> Nothing
    (x, xs) -> Just (x, T.drop 1 xs)

  let branch = decodeText branch'
      filePath = decodeText filePath'
      zkData = Nothing

  Just ZKConfig {..}
