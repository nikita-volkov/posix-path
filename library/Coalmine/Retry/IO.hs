module Coalmine.Retry.IO where

import Coalmine.InternalPrelude hiding (print)
import Coalmine.Retry.Strategy

retry ::
  RetryStrategy ->
  -- | Action to execute on each attempt.
  -- When Right the result is propagated.
  IO (Either err ok) ->
  -- | Action producing the last attempt error and attempt count in case of
  -- retry strategy terminating.
  IO (Either (err, Int) ok)
retry (RetryStrategy retryState retryStep) attempt =
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

retryStateful ::
  RetryStrategy ->
  -- | Initial attempt state.
  state ->
  -- | Action to execute on each attempt.
  (state -> IO (Either state ok)) ->
  IO (Either state ok)
retryStateful (RetryStrategy retryState retryStep) initialAttemptState attemptStep =
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
