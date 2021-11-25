-- |
-- Extra types and aliases.
module Coalmine.Types where

import qualified Coalmine.MultilineTextBuilder as MultilineTextBuilder
import qualified Data.Vector
import qualified Data.Vector.Unboxed
import qualified Text.Builder as TextBuilder
import Prelude

type MultilineTextBuilder = MultilineTextBuilder.Builder

type TextBuilder = TextBuilder.Builder

type UVec = Data.Vector.Unboxed.Vector

type Vec = Data.Vector.Vector
