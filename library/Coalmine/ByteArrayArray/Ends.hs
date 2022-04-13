module Coalmine.ByteArrayArray.Ends where

import Coalmine.InternalPrelude
import qualified Coalmine.VectorExtras.Generic as GVecExtras
import qualified Data.Vector.Unboxed as UVec
import qualified Data.Vector.Unboxed.Mutable as MUVec
import GHC.Exts

-- *

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
