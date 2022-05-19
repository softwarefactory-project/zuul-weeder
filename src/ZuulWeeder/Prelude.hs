-- | The project standard library
module ZuulWeeder.Prelude
  ( module Prelude,

    -- * base
    Generic,
    catMaybes,
    mapMaybe,
    isJust,
    fromMaybe,
    fromRight,
    (&),
    traverse_,
    sort,
    forM_,
    runIdentity,
    liftIO,
    trace,
    elemIndex,
    hPutStrLn,
    stderr,
    orDie,
    fromEither,
    Int64,
    unless,
    when,
    whenM,
    nub,
    lookupEnv,
    NonEmpty,
    threadDelay,

    -- * hashable
    Hashable,
    hash,

    -- * exceptions
    SomeException,
    catchAll,

    -- * clock
    getSec,

    -- * filepath text
    FilePathT (..),
    (</>),
    getPath',
    listDirectory,
    doesDirectoryExist,
    readFileBS,
    readFileText,

    -- * text
    Text,

    -- * containers
    Map,
    Set,

    -- * witch
    Witch.From,
    Witch.from,
    Witch.via,
    Witch.into,

    -- * mtl
    lift,
    State,
    execStateT,
    StateT,
    ExceptT,
    runExceptT,
    throwError,
    except,

    -- * lens
    Control.Lens.over,
    Control.Lens.set,
    Control.Lens.element,
    Control.Lens.preview,
    Control.Lens.use,
    (%=),
    safeGet,

    -- * aeson helpers
    decodeAsList,
    unwrapObject,
    getObjValue,
    getString,

    -- * text-display
    module Data.Text.Display,

    -- * qq
    Data.String.QQ.s,
  )
where

import Control.Concurrent (threadDelay)
import Control.Lens ((%=))
import Control.Lens qualified
import Control.Monad (forM_, unless, when)
import Control.Monad.Catch (SomeException, catchAll)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (State, StateT, execStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (except)
import Data.Aeson (Object, Value (Array, Object, String))
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified as HM
import Data.ByteString qualified as BS
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
-- This import is necessary to bring orphan Lens instance for #labels
import Data.Generics.Labels ()
import Data.Hashable (Hashable, hash)
import Data.Int (Int64)
import Data.List (elemIndex, nub, sort)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import Data.String (IsString)
import Data.String.QQ qualified (s)
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.IO qualified as Text (readFile)
import Data.Vector qualified as V
import Debug.Trace (trace)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.Clock qualified
import System.Directory qualified
import System.Environment (lookupEnv)
import System.FilePath qualified
import System.IO (hPutStrLn, stderr)
import Witch qualified

newtype FilePathT = FilePathT {getPath :: Text}
  deriving newtype (Show, Eq, Ord, IsString, Semigroup, Monoid, Hashable)
  deriving (Display) via (ShowInstance Text)

whenM :: Monad m => m Bool -> m () -> m ()
whenM test action = do
  res <- test
  when res action

safeGet :: Int -> [a] -> Maybe a
safeGet pos xs = Control.Lens.element pos `Control.Lens.preview` xs

getPath' :: FilePathT -> FilePath
getPath' = unpack . getPath

(</>) :: FilePathT -> FilePathT -> FilePathT
FilePathT a </> FilePathT b = FilePathT (pack $ unpack a `System.FilePath.combine` unpack b)

listDirectory :: FilePathT -> IO [FilePathT]
listDirectory (FilePathT fp) = map (FilePathT . pack) <$> System.Directory.listDirectory (unpack fp)

doesDirectoryExist :: FilePathT -> IO Bool
doesDirectoryExist (FilePathT fp) = System.Directory.doesDirectoryExist (unpack fp)

readFileBS :: FilePathT -> IO BS.ByteString
readFileBS (FilePathT fp) = BS.readFile (unpack fp)

readFileText :: FilePathT -> IO Text
readFileText (FilePathT fp) = Text.readFile (unpack fp)

getSec :: IO Int64
getSec = do
  System.Clock.TimeSpec sec _ <- System.Clock.getTime System.Clock.Monotonic
  pure sec

-- | From https://www.haskellforall.com/2021/05/the-trick-to-avoid-deeply-nested-error.html
orDie :: Maybe a -> b -> Either b a
Just a `orDie` _ = Right a
Nothing `orDie` err = Left err

fromEither :: Show a => Either a b -> b
fromEither e = case e of
  Left x -> error (show x)
  Right x -> x

decodeAsList :: Text -> (Text -> a) -> Object -> [a]
decodeAsList k build va = case HM.lookup (Data.Aeson.Key.fromText k) va of
  Just (String x) -> [build x]
  Just (Array xs) -> build . getString <$> sort (V.toList xs)
  Just _va -> error $ "Unexpected " <> Text.unpack k <> " structure: " <> show _va
  Nothing -> []

unwrapObject :: HasCallStack => Value -> Object
unwrapObject va = case va of
  Object hm -> hm
  _ -> error $ "Expecting an Object out of JSON Value: " <> show va

getObjValue :: Text -> Object -> Value
getObjValue k hm = case HM.lookup (Data.Aeson.Key.fromText k) hm of
  Just va -> va
  Nothing -> error $ "Unable to get " <> Text.unpack k <> " from Object: " <> show (HM.keys hm)

getString :: Value -> Text
getString va = case va of
  String str -> str
  _ -> error $ "Expected a String out of JSON value: " <> show va
