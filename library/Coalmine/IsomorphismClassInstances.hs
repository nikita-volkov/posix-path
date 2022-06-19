-- |
-- Instances for the externally defined data-types used across this lib.
module Coalmine.IsomorphismClassInstances where

import Coalmine.InternalPrelude
import qualified Coalmine.TimeExtras.Conversions as TimeConversions
import Data.Ratio ((%))
import qualified Data.Vector.Generic as VectorGeneric
import qualified Data.Vector.Unboxed as VectorUnboxed
import qualified TextBuilderDev

--

instance IsomorphicTo a b => IsomorphicTo (Deque a) (Deque b) where
  to = fmap to

instance IsomorphicTo (Deque a) [a] where
  to = fromList

instance IsomorphicTo (Deque a) (BVec a) where
  to = from @[a] . to

--

instance IsomorphicTo [a] (Deque a) where
  to = toList

--

instance IsomorphicTo (BVec a) (Deque a) where
  to = to . to @[a]
