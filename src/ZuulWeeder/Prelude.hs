{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : ZuulWeeder.Prelude
-- Description : The project Prelude
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
--
-- Maintainer  : tdecacqu@redhat.com, fboucher@redhat.com
-- Stability   : provisional
-- Portability : portable
--
-- This module exports common functions and helpers.
module ZuulWeeder.Prelude
  ( -- * clock
    getSec,
    intervalMilliSec,

    -- * th-env
    gitVersion,

    -- * fast-logger
    Logger,
    info,
    withLogger,

    -- * pretty-simple
    Text.Pretty.Simple.pPrint,
    Text.Pretty.Simple.pShowNoColor,

    -- * exceptions
    Control.Exception.SomeException,
    Control.Exception.try,
    Control.Monad.Catch.catchAll,

    -- * filepath text
    FilePathT (..),
    (</>),
    getPath,
    listDirectory,
    doesDirectoryExist,
    readFileBS,
    readFileText,

    -- * text, bytestring
    Data.Text.Text,
    Data.ByteString.ByteString,

    -- * containers
    Data.Map.Map,
    Data.Set.Set,
    Data.Tree.Forest,
    Data.Tree.Tree (..),

    -- * witch
    Witch.From,
    Witch.from,
    Witch.via,
    Witch.into,
    Witch.unsafeFrom,
    Witch.unsafeInto,

    -- * mtl
    Control.Monad.Trans.lift,
    Control.Monad.Reader.Reader,
    Control.Monad.Reader.ReaderT,
    Control.Monad.Reader.runReaderT,
    Control.Monad.State.State,
    Control.Monad.State.StateT,
    Control.Monad.State.execStateT,
    Control.Monad.Except.ExceptT,
    Control.Monad.Except.runExceptT,
    Control.Monad.Except.throwError,
    Control.Monad.Trans.Except.except,
    Data.Functor.Identity.runIdentity,

    -- * lens
    Control.Lens.set,
    Control.Lens.over,
    Control.Lens.use,
    (%=),

    -- * aeson
    Data.Aeson.FromJSON (..),
    Data.Aeson.FromJSONKey (..),
    Data.Aeson.ToJSON (..),
    Data.Aeson.ToJSONKey (..),
    Data.Aeson.Value (Object),
    Data.Aeson.genericParseJSON,
    Data.Aeson.genericToJSON,
    Data.Aeson.defaultOptions,
    Data.Aeson.omitNothingFields,
    encodeJSON,
    decodeJSON,

    -- * aeson helpers
    Decoder (..),
    decodeFail,
    decodeObject,
    decodeObjectAttribute,
    decodeAsList,
    decodeString,
    decodeList,

    -- * qq
    Data.String.QQ.s,

    -- * with-utf8
    Main.Utf8.withUtf8,

    -- * utilities
    whenM,
    orDie,
    fromEither,

    -- * base concurrent
    Control.Concurrent.forkIO,
    Control.Concurrent.threadDelay,
    Data.IORef.IORef,
    Data.IORef.newIORef,
    Data.IORef.readIORef,
    Data.IORef.writeIORef,
    Control.Concurrent.MVar.MVar,
    Control.Concurrent.MVar.newMVar,
    Control.Concurrent.MVar.modifyMVar,

    -- * base list
    Data.List.sort,
    Data.List.nub,
    Data.List.NonEmpty.NonEmpty,
    Data.List.NonEmpty.nonEmpty,

    -- * base data
    Int64,
    (&),
    Data.Proxy.Proxy (..),
    Data.Bifunctor.first,
    Data.Bool.bool,
    Data.Foldable.traverse_,
    Data.Maybe.catMaybes,
    Data.Maybe.mapMaybe,
    Data.Maybe.isJust,
    Data.Maybe.isNothing,
    Data.Maybe.fromMaybe,
    Data.Either.fromRight,

    -- * base control
    Control.Monad.forM_,
    Control.Monad.unless,
    Control.Monad.when,
    Control.Monad.void,
    Control.Monad.IO.Class.liftIO,
    (<=<),
    (>=>),
    (<|>),

    -- * base debug
    Debug.Trace.trace,
    System.IO.hPutStrLn,
    System.IO.stderr,
    GHC.Stack.HasCallStack,

    -- * base system
    Data.Version.showVersion,
    System.Environment.lookupEnv,
    System.Environment.getArgs,
    System.Timeout.timeout,

    -- * hashable
    Data.Hashable.hash,
    Data.Hashable.Hashable,

    -- * base
    module Prelude,
    Generic,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar qualified
import Control.Exception qualified
import Control.Lens ((%=))
import Control.Lens qualified
import Control.Monad ((<=<), (>=>))
import Control.Monad qualified
import Control.Monad.Catch qualified
import Control.Monad.Except qualified
import Control.Monad.IO.Class qualified
import Control.Monad.Reader qualified
import Control.Monad.State qualified
import Control.Monad.Trans qualified
import Control.Monad.Trans.Except qualified
import Data.Aeson (Object, Value (Array, Object, String))
import Data.Aeson qualified
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified as HM
import Data.Bifunctor qualified
import Data.Bool qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either qualified
import Data.Foldable qualified
import Data.Function ((&))
import Data.Functor.Identity qualified
import Data.Generics.Labels ()
import Data.Hashable qualified
import Data.IORef qualified
import Data.Int
import Data.List qualified
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map (Map)
import Data.Maybe qualified
import Data.Proxy qualified
import Data.Set (Set)
import Data.String (IsString)
import Data.String.QQ qualified (s)
import Data.Text (Text, pack, unpack)
import Data.Text.IO qualified as Text (readFile)
import Data.Tree qualified
import Data.Vector qualified as V
import Data.Version qualified
import Debug.Trace qualified
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Env qualified
import Main.Utf8 qualified (withUtf8)
import System.Clock qualified
import System.Directory qualified
import System.Environment qualified
import System.FilePath qualified
import System.IO qualified
import System.Log.FastLogger qualified
import System.Timeout qualified (timeout)
import Text.Pretty.Simple qualified
import Witch qualified

encodeJSON :: Data.Aeson.ToJSON a => a -> ByteString
encodeJSON = Witch.from . Data.Aeson.encode

decodeJSON :: Data.Aeson.FromJSON a => ByteString -> Either String a
decodeJSON = Data.Aeson.eitherDecodeStrict

-- | The content of the GIT_COMMIT environment variable, default to HEAD.
gitVersion :: Text
gitVersion = Data.Maybe.fromMaybe "HEAD" $$(Language.Haskell.TH.Env.envQ "GIT_COMMIT")

-- | The fast-logger.
newtype Logger = Logger System.Log.FastLogger.TimedFastLogger

-- | Create the logger.
withLogger :: (Logger -> IO a) -> IO a
withLogger cb = do
  tc <- System.Log.FastLogger.newTimeCache "%F %T "
  System.Log.FastLogger.withTimedFastLogger tc l (cb . Logger)
  where
    l = System.Log.FastLogger.LogStderr 1024

-- | Log a message.
info :: Logger -> ByteString -> IO ()
info (Logger logger) msg = logger (\time -> System.Log.FastLogger.toLogStr $ time <> msg <> "\n")

-- | lifted 'Control.Monad.when'
whenM :: Monad m => m Bool -> m () -> m ()
whenM test action = do
  res <- test
  Control.Monad.when res action

-- | A FilePath encoded with UTF-8.
newtype FilePathT = FilePathT Text
  deriving (Generic)
  deriving newtype (Show, Eq, Ord, IsString, Semigroup, Monoid, Data.Hashable.Hashable, Data.Aeson.FromJSON, Data.Aeson.ToJSON)

instance Witch.From FilePathT Text where
  from (FilePathT fp) = fp

-- | Get the string FilePath for external libraries.
getPath :: FilePathT -> FilePath
getPath = unpack . Witch.from

-- | Combine two files path with 'System.FilePath.combine'
(</>) :: FilePathT -> FilePathT -> FilePathT
a </> b = FilePathT . pack $ getPath a `System.FilePath.combine` getPath b

-- | Wrapper for 'System.Directory.listDirectory'.
listDirectory :: FilePathT -> IO [FilePathT]
listDirectory fp = map (FilePathT . pack) <$> System.Directory.listDirectory (getPath fp)

-- | Wrapper for 'System.Directory.doesDirectoryExist'
doesDirectoryExist :: FilePathT -> IO Bool
doesDirectoryExist = System.Directory.doesDirectoryExist . getPath

-- | Wrapper for 'Data.ByteString.readFile'
readFileBS :: FilePathT -> IO BS.ByteString
readFileBS = BS.readFile . getPath

-- | Wrapper for 'Data.Text.IO.readFile'
readFileText :: FilePathT -> IO Text
readFileText = Text.readFile . getPath

-- | Get the clock seconds.
getSec :: IO Int64
getSec = do
  System.Clock.TimeSpec sec _ <- System.Clock.getTime System.Clock.Monotonic
  pure sec

-- | Compute the time interval in milli-seconds ellapsed between now and the provided action.
intervalMilliSec :: IO (IO Int64)
intervalMilliSec = do
  start <- System.Clock.getTime System.Clock.Monotonic
  pure $ do
    end <- System.Clock.getTime System.Clock.Monotonic
    let ns = System.Clock.toNanoSecs $ System.Clock.diffTimeSpec end start
    pure $ fromIntegral (ns `div` 1_000_000)

-- | From https://www.haskellforall.com/2021/05/the-trick-to-avoid-deeply-nested-error.html
orDie :: Maybe a -> b -> Either b a
Just a `orDie` _ = Right a
Nothing `orDie` err = Left err

-- | Die with on the Left case
fromEither :: Show a => Either a b -> b
fromEither e = case e of
  Left x -> error (show x)
  Right x -> x

-- | A convenient wrapper to decode json value with custom error message.
newtype Decoder a = Decoder (Either (Text, Value) a)
  deriving (Traversable)
  deriving newtype (Functor, Applicative, Monad, Show, Foldable)

-- | Decode a json object.
decodeObject :: Value -> Decoder Object
decodeObject = \case
  (Object o) -> pure o
  x -> decodeFail "expecting an object" x

-- | Decode a json object attribute value.
decodeObjectAttribute :: Data.Aeson.Key.Key -> Object -> Decoder Value
decodeObjectAttribute k o = case HM.lookup k o of
  Just v -> pure v
  Nothing -> decodeFail ("can't find key:" <> Data.Aeson.Key.toText k) (Object o)

-- | Decode a json list.
decodeList :: Value -> Decoder [Value]
decodeList = \case
  (Array v) -> pure (V.toList v)
  v -> decodeFail "expected a list" v

-- | Decode a json string.
decodeString :: Value -> Decoder Text
decodeString = \case
  (String v) -> pure v
  v -> decodeFail "Expected a string" v

-- | Decode a json object attribute value as a list.
decodeAsList :: Data.Aeson.Key.Key -> (Text -> a) -> Object -> Decoder [a]
decodeAsList k build va = case HM.lookup k va of
  Just (String x) -> pure [build x]
  Just (Array xs) -> fmap build <$> traverse decodeString (V.toList xs)
  Just _va -> decodeFail ("Unexpected " <> Data.Aeson.Key.toText k) (Object va)
  Nothing -> pure []

-- | Abort a decoder.
decodeFail :: Text -> Value -> Decoder a
decodeFail t v = Decoder (Left (t, v))
