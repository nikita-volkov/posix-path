module Coalmine.Hasql.Decoder where

import Prelude
import Hasql.Decoders
import Reduction (Reduction)
import qualified Reduction


reduceRows :: Reduction a b -> Row a -> Result (Reduction a b)
reduceRows = foldlRows (flip Reduction.feed)
