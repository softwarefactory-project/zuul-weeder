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

    -- * text
    Text,

    -- * containers
    Map,
    Set,

    -- * witch
    Witch.From,
    Witch.from,

    -- * mtl
    lift,
    State,
    execStateT,
    StateT,

    -- * lens
    Control.Lens.over,
    Control.Lens.set,
    (%=),

    -- * text-display
    module Data.Text.Display
  )
where

import Control.Monad (forM_)
import Control.Monad.Trans (lift)
import Data.Generics.Labels ()
import Control.Monad.State (StateT, State, execStateT)
import Data.Foldable (traverse_)
import Data.List (sort)
import Data.Function ((&))
import Control.Lens ((%=))
import Control.Lens qualified
import Data.Maybe (fromMaybe, mapMaybe, isJust)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Text.Display
import Witch qualified
