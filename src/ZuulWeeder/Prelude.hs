-- | The project standard library
module ZuulWeeder.Prelude
  ( module Prelude,

    -- * base
    Generic,
    mapMaybe,
    isJust,
    fromMaybe,
    (&),
    traverse_,
    sort,
    forM_,
    runIdentity,
    liftIO,
    trace,
    elemIndex,

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

import Control.Lens ((%=))
import Control.Lens qualified
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (State, StateT, execStateT)
import Control.Monad.Trans (lift)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
-- This import is necessary to bring orphan Lens instance for #labels
import Data.Generics.Labels ()
import Data.List (elemIndex, sort)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text, pack, unpack)
import Data.Text.Display
import Debug.Trace (trace)
import GHC.Generics (Generic)
import System.Directory qualified
import System.FilePath qualified
import Witch qualified

newtype FilePathT = FilePathT {getPath :: Text}
  deriving newtype (Show, Eq, Ord, IsString, Semigroup, Monoid)
  deriving (Display) via (ShowInstance Text)

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
