module Coalmine.RetryStrategy where

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

runRetryStrategyInIO ::
  RetryStrategy ->
  IO (Maybe a) ->
  IO (Maybe a)
runRetryStrategyInIO (RetryStrategy retryState retryStep) attempt =
  go retryState
  where
    go !retryState =
      attempt >>= \case
        Just a -> return (Just a)
        Nothing -> do
          case retryStep retryState of
            Nothing -> return Nothing
            Just (delayInMilliseconds, retryState) -> do
              threadDelay $ 1000 * delayInMilliseconds
              go retryState

-- | Execute an iteration of the strategy,
-- producing a strategy for the next iteration.
step :: RetryStrategy -> Maybe (Int, RetryStrategy)
step (RetryStrategy state step) =
  case step state of
    Just (emission, state) -> Just (emission, RetryStrategy state step)
    Nothing -> Nothing

growFromByFactorUntil ::
  -- | Initial amount of milliseconds.
  Int ->
  -- | Factor.
  Double ->
  -- | Max milliseconds. Inclusive.
  Int ->
  RetryStrategy
growFromByFactorUntil init factor max =
  RetryStrategy init step
  where
    step lastMillis =
      if lastMillis <= max
        then Just (lastMillis, round (factor * fromIntegral lastMillis))
        else Nothing
