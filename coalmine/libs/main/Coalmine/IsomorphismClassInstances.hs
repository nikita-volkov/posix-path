-- |
-- Instances for the externally defined data-types used across this lib.
module Coalmine.IsomorphismClassInstances where

import Coalmine.InternalPrelude
import Coalmine.TimeExtras.Conversions qualified as TimeConversions
import Data.Ratio ((%))
import Data.Vector.Generic qualified as VectorGeneric
import Data.Vector.Unboxed qualified as VectorUnboxed
import TextBuilderDev qualified

--

instance (IsomorphicTo a b) => IsomorphicTo (Deque a) (Deque b) where
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
