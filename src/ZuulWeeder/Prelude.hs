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

    -- * fast-logger
    Logger,
    info,
    withLogger,

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

    -- * witch
    Witch.From,
    Witch.from,
    Witch.via,
    Witch.into,

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
    Data.Aeson.Value,
    decodeAsList,
    unwrapObject,
    getObjValue,
    getString,

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

    -- * base debug
    Debug.Trace.trace,
    System.IO.hPutStrLn,
    System.IO.stderr,

    -- * base system
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

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar qualified
import Control.Exception qualified
import Control.Lens ((%=))
import Control.Lens qualified
import Control.Monad qualified
import Control.Monad.Catch qualified
import Control.Monad.Except qualified
import Control.Monad.IO.Class qualified
import Control.Monad.Reader qualified
import Control.Monad.State qualified
import Control.Monad.Trans qualified
import Control.Monad.Trans.Except qualified
import Data.Aeson (Object, Value (Array, Object, String))
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified as HM
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
import Data.Set (Set)
import Data.String (IsString)
import Data.String.QQ qualified (s)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text (readFile)
import Data.Vector qualified as V
import Debug.Trace qualified
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Main.Utf8 qualified (withUtf8)
import System.Clock qualified
import System.Directory qualified
import System.Environment qualified
import System.FilePath qualified
import System.IO qualified
import System.Log.FastLogger qualified
import System.Timeout qualified (timeout)
import Witch qualified

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
  deriving newtype (Show, Eq, Ord, IsString, Semigroup, Monoid, Data.Hashable.Hashable)

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

-- | Decode a list of values.
decodeAsList :: HasCallStack => Text -> (Text -> a) -> Object -> [a]
decodeAsList k build va = case HM.lookup (Data.Aeson.Key.fromText k) va of
  Just (String x) -> [build x]
  Just (Array xs) -> build . getString <$> Data.List.sort (V.toList xs)
  Just _va -> error $ "Unexpected " <> Text.unpack k <> " structure: " <> show _va
  Nothing -> []

-- | Unwrap a json object
unwrapObject :: HasCallStack => Value -> Object
unwrapObject va = case va of
  Object hm -> hm
  _ -> error $ "Expecting an Object out of JSON Value: " <> show va

-- | Get an json object attribute value.
getObjValue :: HasCallStack => Text -> Object -> Value
getObjValue k hm = case HM.lookup (Data.Aeson.Key.fromText k) hm of
  Just va -> va
  Nothing -> error $ "Unable to get " <> Text.unpack k <> " from Object: " <> show (HM.keys hm)

-- | Get a json string.
getString :: HasCallStack => Value -> Text
getString va = case va of
  String str -> str
  _ -> error $ "Expected a String out of JSON value: " <> show va
