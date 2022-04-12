--- This is expected to run on a dump done via
--- python3 /var/log/zuul/zk-dump.py --cert /etc/zuul/ssl/zookeeper.crt --key /etc/zuul/ssl/zookeeper.key --ca /etc/zuul/ssl/zk-ca.pem managesf.sftests.com:2281 --decompress /var/log/zuul/zk-dump
--- then
--- S.print $ walk "/var/log/zuul/zk-dump"

module Zuul.ZKDump
  ( walkConfigNodes,
    mkZKConfig,
    ZKConfig (..),
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)
import Data.Aeson (Value)
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
import qualified Data.Yaml as Y (ParseException, decodeEither')
import Network.URI.Encode (decodeText)
import qualified Streaming.Prelude as S
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

data ZKConfig = ZKConfig
  { provider :: T.Text,
    project :: T.Text,
    branch :: T.Text,
    filePath :: T.Text,
    zkJSONData :: Value
  }
  deriving (Show, Eq)

data ConfigError
  = ReadError SomeException
  | DecodeError Y.ParseException
  | InvalidPath FilePath
  deriving (Show)

type ZKConfigStream = S.Stream (S.Of (Either ConfigError ZKConfig)) IO ()

walkConfigNodes :: FilePath -> ZKConfigStream
walkConfigNodes dumpPath = walkRecursive $ dumpPath </> "zuul" </> "config" </> "cache"
  where
    walkRecursive :: FilePath -> ZKConfigStream
    walkRecursive curPath = do
      tree <- lift $ listDirectory curPath
      forM_ tree $ \path -> do
        let fullPath = curPath </> path
        isDirectory <- lift $ doesDirectoryExist fullPath
        if isDirectory
          then walkRecursive fullPath
          else do
            when ("/0000000000/ZKDATA" `T.isSuffixOf` T.pack fullPath) $
              lift (runExceptT (handleConfig fullPath)) >>= S.yield

    handleConfig :: FilePath -> ExceptT ConfigError IO ZKConfig
    handleConfig fullPath = do
      zkData <- readZKData fullPath
      zkJSONData <- case Y.decodeEither' zkData of
        Left err -> throwError $ DecodeError err
        Right content -> pure content

      case mkZKConfig zkJSONData fullPath of
        Just config -> pure config
        Nothing -> throwError $ InvalidPath fullPath

    readZKData :: FilePath -> ExceptT ConfigError IO ByteString
    readZKData path = do
      zkDataE <- lift $ try $ BS.readFile path
      case zkDataE of
        Left err -> throwError $ ReadError err
        Right content -> pure content

mkZKConfig :: Value -> FilePath -> Maybe ZKConfig
mkZKConfig zkJSONData path = do
  [_, _, filePath', _, branch', providerPath'] <-
    pure $
      take 6 $ reverse $ T.split (== '/') (T.pack path)
  (provider, project) <- case T.breakOn "/" $ decodeText providerPath' of
    (_, "") -> Nothing
    (x, xs) -> Just (x, T.drop 1 xs)

  let branch = decodeText branch'
      filePath = decodeText filePath'

  Just ZKConfig {..}
