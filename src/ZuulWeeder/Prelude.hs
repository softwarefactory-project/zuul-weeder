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
    when,
    whenM,
    nub,

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

    -- * text
    Text,

    -- * containers
    Map,
    Set,

    -- * witch
    Witch.From,
    Witch.from,
    Witch.via,

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
    (%=),
    safeGet,

    -- * text-display
    module Data.Text.Display,
  )
where

import Data.Either (fromRight)
import Control.Lens ((%=))
import Control.Lens qualified
import Control.Monad (when, forM_)
import Control.Monad.Catch (catchAll, SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (State, StateT, execStateT)
import Control.Monad.Trans.Except (except)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
-- This import is necessary to bring orphan Lens instance for #labels
import Data.Generics.Labels ()
import Data.List (elemIndex, sort, nub)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, mapMaybe, catMaybes)
import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text, pack, unpack)
import Data.Text.Display
import Debug.Trace (trace)
import GHC.Generics (Generic)
import System.Directory qualified
import System.FilePath qualified
import Witch qualified
import System.IO (hPutStrLn, stderr)
import qualified System.Clock

newtype FilePathT = FilePathT {getPath :: Text}
  deriving newtype (Show, Eq, Ord, IsString, Semigroup, Monoid)
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
