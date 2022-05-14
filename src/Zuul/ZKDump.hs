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
import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Aeson (Value, eitherDecodeFileStrict)
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import qualified Data.Yaml
import qualified Network.URI.Encode
import qualified Streaming.Prelude as S
import ZuulWeeder.Prelude

data ZKConfig = ZKConfig
  { provider :: Text,
    project :: Text,
    branch :: Text,
    filePath :: FilePathT,
    zkJSONData :: Value
  }
  deriving (Show, Eq)

newtype ZKSystemConfig = ZKSystemConfig Value deriving (Show, Eq)

data ConfigError
  = ReadError SomeException
  | DecodeError Data.Yaml.ParseException
  | InvalidPath FilePathT
  deriving (Show)

type ZKConfigStream = S.Stream (S.Of (Either ConfigError ZKConfig)) IO ()

walkConfigNodes :: FilePathT -> ZKConfigStream
walkConfigNodes dumpPath = walkRecursive $ dumpPath </> "zuul/config/cache"
  where
    walkRecursive :: FilePathT -> ZKConfigStream
    walkRecursive curPath = do
      tree <- lift $ listDirectory curPath
      forM_ tree $ \path -> do
        let fullPath = curPath </> path
        isDirectory <- lift $ doesDirectoryExist fullPath
        if isDirectory
          then walkRecursive fullPath
          else do
            when ("/0000000000/ZKDATA" `Text.isSuffixOf` getPath fullPath) $
              lift (runExceptT (handleConfig fullPath)) >>= S.yield

    handleConfig :: FilePathT -> ExceptT ConfigError IO ZKConfig
    handleConfig fullPath = do
      zkData <- readZKData fullPath
      zkJSONData <- case Data.Yaml.decodeEither' zkData of
        Left err -> throwError $ DecodeError err
        Right content -> pure content

      case mkZKConfig zkJSONData fullPath of
        Just config -> pure config
        Nothing -> throwError $ InvalidPath fullPath

    readZKData :: FilePathT -> ExceptT ConfigError IO ByteString
    readZKData path = do
      zkDataE <- lift $ try $ readFileBS path
      case zkDataE of
        Left err -> throwError $ ReadError err
        Right content -> pure content

readSystemConfig :: FilePathT -> IO (Either String ZKSystemConfig)
readSystemConfig dumpPath = do
  configE <- readC
  pure $ ZKSystemConfig <$> configE
  where
    readC :: IO (Either String Value)
    readC = eitherDecodeFileStrict . getPath' $ dumpPath </> "zuul/system/conf/0000000000/ZKDATA"

mkZKConfig :: Value -> FilePathT -> Maybe ZKConfig
mkZKConfig zkJSONData path = do
  [_, _, filePath', _, branch', Network.URI.Encode.decodeText -> providerPath'] <-
    pure $
      take 6 $ reverse $ Text.split (== '/') (getPath path)
  (provider, project) <- case Text.breakOn "/" providerPath' of
    (_, "") -> Nothing
    (x, xs) -> Just (x, Text.drop 1 xs)

  let branch = Network.URI.Encode.decodeText branch'
      filePath = FilePathT $ Network.URI.Encode.decodeText filePath'

  Just ZKConfig {..}
