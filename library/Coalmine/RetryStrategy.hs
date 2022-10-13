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
  -- | Action to execute on each attempt.
  IO (Either err ok) ->
  -- | Action producing the last attempt error and attempt count in case of
  -- retry strategy terminating..
  IO (Either (err, Int) ok)
runRetryStrategyInIO (RetryStrategy retryState retryStep) attempt =
  go retryState 0
  where
    go !retryState !attemptCount =
      attempt >>= \case
        Right ok -> return (Right ok)
        Left err -> do
          case retryStep retryState of
            Nothing -> return (Left (err, attemptCount))
            Just (delayInMilliseconds, retryState) -> do
              threadDelay $ 1000 * delayInMilliseconds
              go retryState (succ attemptCount)

runRetryStrategyInIOStatefully ::
  RetryStrategy ->
  -- | Initial attempt state.
  state ->
  -- | Action to execute on each attempt.
  (state -> IO (Either state ok)) ->
  IO (Either state ok)
runRetryStrategyInIOStatefully (RetryStrategy retryState retryStep) initialAttemptState attemptStep =
  go initialAttemptState retryState
  where
    go !attemptState !retryState =
      attemptStep attemptState >>= \case
        Right ok -> return (Right ok)
        Left nextAttemptState -> case retryStep retryState of
          Nothing -> return (Left attemptState)
          Just (delayInMilliseconds, nextRetryState) -> do
            threadDelay $ 1000 * delayInMilliseconds
            go nextAttemptState nextRetryState

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
