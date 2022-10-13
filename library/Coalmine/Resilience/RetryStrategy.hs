-- | Pure API focused on retry strategy.
module Coalmine.Resilience.RetryStrategy where

import Coalmine.InternalPrelude hiding (print)

data RetryStrategy
  = forall state.
    RetryStrategy
      state
      -- ^ Initial state.
      (state -> Maybe (Int, state))
      -- ^ Function possibly modifying the strategy state and producing a
      -- pause duration in milliseconds. When results in nothing it implies a
      -- terminal condition of the strategy. It's up to the interpreter to
      -- decide what that means. Typically interpreting it as a fatal error
      -- due to which the app should stop running.

instance Semigroup RetryStrategy where
  RetryStrategy lState lStep <> RetryStrategy rState rStep =
    RetryStrategy (Left lState) $ \case
      Left lState -> case lStep lState of
        Just (emission, lState) -> Just (emission, Left lState)
        Nothing -> error "TODO"
      Right rState -> error "TODO"

-- | Execute an iteration of the strategy,
-- producing a strategy for the next iteration.
step :: RetryStrategy -> Maybe (Int, RetryStrategy)
step (RetryStrategy state step) =
  case step state of
    Just (emission, state) -> Just (emission, RetryStrategy state step)
    Nothing -> Nothing

growFromToByFactor ::
  -- | Initial amount of milliseconds.
  Int ->
  -- | Max milliseconds. Inclusive.
  Int ->
  -- | Factor.
  Double ->
  RetryStrategy
growFromToByFactor init max factor =
  RetryStrategy init step
  where
    step lastMillis =
      if lastMillis <= max
        then Just (lastMillis, round (factor * fromIntegral lastMillis))
        else Nothing
