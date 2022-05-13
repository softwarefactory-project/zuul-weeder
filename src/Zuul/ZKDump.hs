--- This is expected to run on a dump done via
--- python3 /var/log/zuul/zk-dump.py --cert /etc/zuul/ssl/zookeeper.crt --key /etc/zuul/ssl/zookeeper.key --ca /etc/zuul/ssl/zk-ca.pem managesf.sftests.com:2281 --decompress /var/log/zuul/zk-dump
--- then
--- S.print $ walk "/var/log/zuul/zk-dump"

module Zuul.ZKDump
  ( walkConfigNodes,
    mkZKConfig,
    readSystemConfig,
    ZKConfig (..),
    ConfigError (..),
    ZKSystemConfig (..),
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)
import Data.Aeson (Value, eitherDecodeFileStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml
import qualified Network.URI.Encode
import qualified Streaming.Prelude as S
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

data ZKConfig = ZKConfig
  { provider :: Text,
    project :: Text,
    branch :: Text,
    filePath :: Text,
    zkJSONData :: Value
  }
  deriving (Show, Eq)

newtype ZKSystemConfig = ZKSystemConfig Value deriving (Show, Eq)

data ConfigError
  = ReadError SomeException
  | DecodeError Data.Yaml.ParseException
  | InvalidPath FilePath
  deriving (Show)

type ZKConfigStream = S.Stream (S.Of (Either ConfigError ZKConfig)) IO ()

walkConfigNodes :: FilePath -> ZKConfigStream
walkConfigNodes dumpPath = walkRecursive $ dumpPath </> "zuul/config/cache"
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
            when ("/0000000000/ZKDATA" `Text.isSuffixOf` Text.pack fullPath) $
              lift (runExceptT (handleConfig fullPath)) >>= S.yield

    handleConfig :: FilePath -> ExceptT ConfigError IO ZKConfig
    handleConfig fullPath = do
      zkData <- readZKData fullPath
      zkJSONData <- case Data.Yaml.decodeEither' zkData of
        Left err -> throwError $ DecodeError err
        Right content -> pure content

      case mkZKConfig zkJSONData fullPath of
        Just config -> pure config
        Nothing -> throwError $ InvalidPath fullPath

    readZKData :: FilePath -> ExceptT ConfigError IO ByteString
    readZKData path = do
      zkDataE <- lift $ try $ Data.ByteString.readFile path
      case zkDataE of
        Left err -> throwError $ ReadError err
        Right content -> pure content

readSystemConfig :: FilePath -> IO (Either String ZKSystemConfig)
readSystemConfig dumpPath = do
  configE <- readC
  pure $ ZKSystemConfig <$> configE
  where
    readC :: IO (Either String Value)
    readC = eitherDecodeFileStrict $ dumpPath </> "zuul/system/conf/0000000000/ZKDATA"

mkZKConfig :: Value -> FilePath -> Maybe ZKConfig
mkZKConfig zkJSONData path = do
  [_, _, filePath', _, branch', Network.URI.Encode.decodeText -> providerPath'] <-
    pure $
      take 6 $ reverse $ Text.split (== '/') (Text.pack path)
  (provider, project) <- case Text.breakOn "/" providerPath' of
    (_, "") -> Nothing
    (x, xs) -> Just (x, Text.drop 1 xs)

  let branch = Network.URI.Encode.decodeText branch'
      filePath = Network.URI.Encode.decodeText filePath'

  Just ZKConfig {..}
