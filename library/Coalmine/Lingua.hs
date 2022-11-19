-- | Classes for various conversion between data-structures and general operations.
module Coalmine.Lingua where

import Coalmine.InternalPrelude
import Data.Text qualified
import Data.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified

type family Strict a

type instance
  Strict Data.Text.Lazy.Text =
    Data.Text.Text

class CanBeStrict a where
  toStrict :: a -> Strict a

type family Lazy a

class CanBeLazy a where
  toLazy :: a -> Lazy a

-- | A finalized representation of some structure that is
-- intended for modification.
--
-- Usually such data-structure is better optimized for read-operations.
-- E.g., accessing metainformation like length.
type family Frozen a

type instance
  Frozen Data.Text.Text =
    Data.Text.Lazy.Builder.Builder

class Freezes a where
  freeze :: a -> Frozen a

type family Melted a

class Melts a where
  melt :: a -> Melted a

class HasLength a where
  length :: a -> Int
