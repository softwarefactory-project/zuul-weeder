--- This is expected to run on a dump done via
--- python3 /var/log/zuul/zk-dump.py --cert /etc/zuul/ssl/zookeeper.crt --key /etc/zuul/ssl/zookeeper.key --ca /etc/zuul/ssl/zk-ca.pem managesf.sftests.com:2281 --decompress /var/log/zuul/zk-dump
--- then
--- S.print $ walk "/var/log/zuul/zk-dump"

module Zuul.ZKDump
  ( walk,
    mkZKConfig,
    ZKConfig (..),
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (readFile)
import qualified Data.Text as T
  ( Text,
    breakOn,
    drop,
    isSuffixOf,
    pack,
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
    zkData :: Maybe ByteString
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
                Nothing -> do
                  liftIO . putStrLn $ "Unable to handle ZK path " <> fullPath
                  pure ()
                Just zc -> do
                  zkDataM <- liftIO $ readZKData fullPath
                  S.yield $ zc {zkData = zkDataM}
    readZKData :: FilePath -> IO (Maybe ByteString)
    readZKData path = do
      zkDataE <- try $ BS.readFile path :: IO (Either SomeException ByteString)
      case zkDataE of
        Left err -> do
          putStrLn $ "Unable to read content from " <> path <> " due to: " <> show err
          pure Nothing
        Right content -> pure $ Just content

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
