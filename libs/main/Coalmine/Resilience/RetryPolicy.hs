-- | Pure API focused on retry strategy.
module Coalmine.Resilience.RetryPolicy where

import Coalmine.InternalPrelude hiding (print)

newtype RetryPolicy
  = RetryPolicy [Int]
  deriving (Semigroup, Monoid)

-- | Execute an iteration of the strategy,
-- producing a strategy for the next iteration.
step :: RetryPolicy -> Maybe (Int, RetryPolicy)
step (RetryPolicy list) =
  case list of
    h : t -> Just (h, RetryPolicy t)
    _ -> Nothing

growFromToByFactor ::
  -- | Initial amount of milliseconds.
  Int ->
  -- | Max milliseconds. Inclusive.
  Int ->
  -- | Factor.
  Double ->
  RetryPolicy
growFromToByFactor init max factor =
  RetryPolicy $ go init
  where
    go lastMillis =
      if lastMillis <= max
        then lastMillis : go (round (factor * fromIntegral lastMillis))
        else []
