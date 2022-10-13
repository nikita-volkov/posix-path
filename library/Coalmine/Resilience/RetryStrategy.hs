-- | Pure API focused on retry strategy.
module Coalmine.Resilience.RetryStrategy where

import Coalmine.InternalPrelude hiding (print)

newtype RetryStrategy
  = RetryStrategy [Int]
  deriving (Semigroup, Monoid)

-- | Execute an iteration of the strategy,
-- producing a strategy for the next iteration.
step :: RetryStrategy -> Maybe (Int, RetryStrategy)
step (RetryStrategy list) =
  case list of
    h : t -> Just (h, RetryStrategy t)
    _ -> Nothing

growFromToByFactor ::
  -- | Initial amount of milliseconds.
  Int ->
  -- | Max milliseconds. Inclusive.
  Int ->
  -- | Factor.
  Double ->
  RetryStrategy
growFromToByFactor init max factor =
  RetryStrategy $ go init
  where
    go lastMillis =
      if lastMillis <= max
        then lastMillis : go (round (factor * fromIntegral lastMillis))
        else []
