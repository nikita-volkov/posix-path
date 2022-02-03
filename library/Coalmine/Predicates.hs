module Coalmine.Predicates where

import Coalmine.Prelude hiding (all, both, either)

-- * Combinators

{-# INLINE either #-}
either :: (a -> Bool) -> (a -> Bool) -> a -> Bool
either l r x = l x || r x

{-# INLINE both #-}
both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both l r x = l x && r x

{-# INLINE all #-}
all :: [a -> Bool] -> a -> Bool
all = getPredicate . mconcat . coerce
