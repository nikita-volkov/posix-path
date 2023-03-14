{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Instances for the externally defined data-types used across this lib.
module Coalmine.IsomorphismClassInstances where

import Coalmine.InternalPrelude

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
