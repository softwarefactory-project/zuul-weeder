-- |
-- Module      : Zuul.ZooKeeper
-- Description : Helpers for ZooKeeper
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
--
-- Maintainer  : tdecacqu@redhat.com, fboucher@redhat.com
-- Stability   : provisional
-- Portability : portable
--
-- This module contains the logic to load data from ZooKeeper
module Zuul.ZooKeeper
  ( -- * Data acquision fetcher
    ZKConnection (..),
    fetchConfigs,

    -- * File parser
    walkConfigNodes,
    ZKFile (..),
    ConfigError (..),

    -- * Helper to load the tenants configuration
    readTenantsConfig,
    ZKTenantsConfig (..),

    -- * Test helpers
    mkZKFile,
  )
where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Text qualified as Text
import Data.Yaml qualified
import Network.URI.Encode qualified
import Streaming.Prelude qualified as S
import System.Directory qualified
import System.Process.Typed qualified
import ZuulWeeder.Prelude

-- | A config file read from ZooKeeper.
data ZKFile = ZKFile
  { -- | The provider name.
    provider :: Text,
    -- | The project name.
    project :: Text,
    -- | The branch name.
    branch :: Text,
    -- | The configuration file path, .e.g ".zuul.d/jobs.yaml".
    filePath :: FilePathT,
    -- | The file path on the host file system, for debugging purpose.
    fullPath :: FilePathT,
    -- | The JSON Value.
    zkJSONData :: Value
  }
  deriving (Show, Eq)

-- | The list of config error that can happens when loading the configuration.
data ConfigError
  = -- | System level exception, e.g. ENOENT
    ReadError Text
  | -- | YAML decoding error
    YamlError Text
  | -- | The path is missing component, e.g. branch name
    InvalidPath
  | AmbiguousName Text
  | -- | A decoding error
    DecodeError FilePathT Text Value
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Read all the configuration found at the given path
walkConfigNodes ::
  -- | The ZooKeeper dump location
  FilePathT ->
  -- | A stream of configuration file
  S.Stream (S.Of (Either ConfigError ZKFile)) IO ()
walkConfigNodes dumpPath = walkRecursive $ dumpPath </> "zuul/config/cache"
  where
    walkRecursive curPath = do
      tree <- lift $ listDirectory curPath
      forM_ tree $ \path -> do
        let fullPath = curPath </> path
        isDirectory <- lift $ doesDirectoryExist fullPath
        if isDirectory
          then walkRecursive fullPath
          else do
            when ("/0000000000/ZKDATA" `Text.isSuffixOf` from fullPath) $
              lift (runExceptT (handleConfig fullPath)) >>= S.yield

    handleConfig :: FilePathT -> ExceptT ConfigError IO ZKFile
    handleConfig fullPath = do
      zkData <- readZKData fullPath
      zkJSONData <- case Data.Yaml.decodeEither' zkData of
        Left err -> throwError $ YamlError (from $ show err)
        Right content -> pure content

      case mkZKFile zkJSONData fullPath of
        Just config -> pure config
        Nothing -> throwError InvalidPath

    readZKData :: FilePathT -> ExceptT ConfigError IO ByteString
    readZKData path = do
      zkDataE <- lift $ try $ readFileBS path
      case zkDataE of
        Left err -> throwError $ ReadError (from $ show (err :: SomeException))
        Right content -> pure content

-- | The tenant configuration value read from ZooKeeper. To be decoded by 'Zuul.Tenant.decodeTenantsConfig'
newtype ZKTenantsConfig = ZKTenantsConfig Value deriving (Show, Eq)

-- | Read the main.yaml from the ZooKeeper dump
readTenantsConfig ::
  -- | The ZooKeeper dump location
  FilePathT ->
  -- | The object to be decoded by "Zuul.Tenant.decodeTenantsConfig"
  ExceptT Text IO ZKTenantsConfig
readTenantsConfig dumpPath =
  ZKTenantsConfig <$> readC
  where
    readC :: ExceptT Text IO Value
    readC = do
      ve <- lift $ eitherDecodeFileStrict . getPath $ dumpPath </> "zuul/system/conf/0000000000/ZKDATA"
      case ve of
        Left e -> throwError (Text.pack e)
        Right x -> pure x

-- | Creates a 'ZKFile' for testing purpose.
mkZKFile :: Value -> FilePathT -> Maybe ZKFile
mkZKFile zkJSONData path = do
  [_, _, filePath', _, branch', Network.URI.Encode.decodeText -> providerPath'] <-
    pure $
      take 6 $
        reverse $
          Text.split (== '/') (from path)
  (provider, project) <- case Text.breakOn "/" providerPath' of
    (_, "") -> Nothing
    (x, xs) -> Just (x, Text.drop 1 xs)

  let branch = Network.URI.Encode.decodeText branch'
      filePath = FilePathT $ Network.URI.Encode.decodeText filePath'
      fullPath = path

  Just ZKFile {provider, project, branch, filePath, fullPath, zkJSONData}

dumpScript :: System.Process.Typed.StreamSpec 'System.Process.Typed.STInput ()
dumpScript =
  System.Process.Typed.byteStringInput
    [s|
import zlib, sys, os, kazoo.client

def getTree(path):
    try:
        data, _ = client.get(path)
        data = zlib.decompress(data)
    except Exception:
        pass
    os.makedirs(root + path)
    with open(root + path + '/ZKDATA', 'wb') as f:
        f.write(data)
    for child in client.get_children(path):
        getTree(path + '/' + child)

[root, host, key, cert, ca] = sys.argv[1:]
client = kazoo.client.KazooClient(host, use_ssl=True, keyfile=key, certfile=cert, ca=ca)
client.start()
getTree("/zuul/config/cache")
getTree("/zuul/system/conf")
|]

-- | The ZooKeeper connection settings: hostname and tls paths.
newtype ZKConnection = ZKConnection [Text] deriving (Eq, Show)

-- | Dump the configuration found in ZooKeeper
fetchConfigs :: Logger -> FilePathT -> ZKConnection -> ExceptT Text IO ()
fetchConfigs logger dataDir (ZKConnection zkConf) = do
  exitCode <- lift $ do
    whenM (doesDirectoryExist dataDir) $ do
      info logger $ "Removing " <> encodeUtf8 (from dataDir)
      System.Directory.removeDirectoryRecursive $ getPath dataDir
    info logger $ "Dumping with " <> encodeUtf8 (from $ show zkConf)
    System.Process.Typed.runProcess process
  case exitCode of
    System.Process.Typed.ExitSuccess -> pure ()
    System.Process.Typed.ExitFailure _ -> throwError "fetchConfig failed!"
  where
    args = "-" : getPath dataDir : map Text.unpack zkConf
    process = System.Process.Typed.setStdin dumpScript $ System.Process.Typed.proc "python" args
