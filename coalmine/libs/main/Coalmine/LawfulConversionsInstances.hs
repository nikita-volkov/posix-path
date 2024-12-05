{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Instances for the externally defined data-types used across this lib.
module Coalmine.LawfulConversionsInstances where

import Coalmine.InternalPrelude

--

instance (IsSome a b) => IsSome (Deque a) (Deque b) where
  to = fmap to
  maybeFrom = traverse maybeFrom

--

instance IsSome [a] (Deque a) where
  to = toList

instance IsSome (Deque a) [a] where
  to = fromList

instance IsMany [a] (Deque a)

instance IsMany (Deque a) [a]

instance Is [a] (Deque a)

instance Is (Deque a) [a]

--

instance IsSome (Deque a) (BVec a) where
  to = from @[a] . to

instance IsSome (BVec a) (Deque a) where
  to = to . to @[a]

instance IsMany (BVec a) (Deque a)

instance IsMany (Deque a) (BVec a)

instance Is (BVec a) (Deque a)

instance Is (Deque a) (BVec a)
