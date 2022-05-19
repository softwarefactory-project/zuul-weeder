-- | Data acquisition from ZooKeeper
module Zuul.ZooKeeper
  ( -- * fetch data
    ZKConnection (..),
    dumpZKConfig,

    -- * parser
    walkConfigNodes,
    mkZKConfig,
    readSystemConfig,
    ZKConfig (..),
    ConfigError (..),
    ZKSystemConfig (..),
  )
where

import Control.Exception (try)
import Data.Aeson (Value, eitherDecodeFileStrict)
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import Data.Yaml qualified
import Network.URI.Encode qualified
import Streaming.Prelude qualified as S
import System.Directory qualified
import System.Process.Typed qualified
import ZuulWeeder.Prelude

data ZKConfig = ZKConfig
  { provider :: Text,
    project :: Text,
    branch :: Text,
    filePath :: FilePathT,
    fullPath :: FilePathT,
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

readSystemConfig :: FilePathT -> ExceptT Text IO ZKSystemConfig
readSystemConfig dumpPath =
  ZKSystemConfig <$> readC
  where
    readC :: ExceptT Text IO Value
    readC = do
      ve <- lift $ eitherDecodeFileStrict . getPath' $ dumpPath </> "zuul/system/conf/0000000000/ZKDATA"
      case ve of
        Left e -> throwError (Text.pack e)
        Right x -> pure x

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
      fullPath = path

  Just ZKConfig {..}

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

newtype ZKConnection = ZKConnection [Text] deriving (Eq, Show)

-- | Dump the configuration found in ZooKeeper
dumpZKConfig :: Logger -> FilePathT -> ZKConnection -> ExceptT Text IO ()
dumpZKConfig logger dataDir (ZKConnection zkConf) = do
  exitCode <- lift $ do
    whenM (doesDirectoryExist dataDir) $ do
      info logger $ "Removing " <> from (getPath dataDir)
      System.Directory.removeDirectoryRecursive $ getPath' dataDir
    info logger $ "Dumping with " <> from (show zkConf)
    System.Process.Typed.runProcess process
  case exitCode of
    System.Process.Typed.ExitSuccess -> pure ()
    System.Process.Typed.ExitFailure _ -> throwError "dumpZKConfig failed!"
  where
    args = "-" : getPath' dataDir : map Text.unpack zkConf
    process = System.Process.Typed.setStdin dumpScript $ System.Process.Typed.proc "python" args
