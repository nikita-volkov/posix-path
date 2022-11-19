module Coalmine.ByteArrayArray.Ends where

import Coalmine.InternalPrelude
import Coalmine.VectorExtras.Generic qualified as GVecExtras
import Data.Vector.Unboxed qualified as UVec
import Data.Vector.Unboxed.Mutable qualified as MUVec
import GHC.Exts

-- * --

newtype Ends = Ends (UVec Int)

instance Semigroup Ends where
  (<>) = error "TODO"

instance Monoid Ends where
  mempty = Ends mempty
  mconcat list = Ends $ GVecExtras.initialized totalLength populate
    where
      totalLength = getSum $ foldMap (Sum . UVec.length . coerce) $ list
      populate mv =
        error "TODO: Map all elements. No hope for vec-copying, since we need to modify it"
