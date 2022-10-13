module Coalmine.Resilience.IO where

import Coalmine.InternalPrelude hiding (print)
import Coalmine.Resilience.RetryStrategy

retry ::
  RetryStrategy ->
  -- | Action to execute on each attempt.
  -- When Right the result is propagated.
  IO (Either err ok) ->
  -- | Action producing the last attempt error and attempt count in case of
  -- retry strategy terminating.
  IO (Either (err, Int) ok)
retry (RetryStrategy pauses) attempt =
  go pauses 0
  where
    go pauses !attemptCount =
      attempt >>= \case
        Left err ->
          case pauses of
            pausesHead : pausesTail -> do
              threadDelay $ 1000 * pausesHead
              go pausesTail (succ attemptCount)
            [] -> return (Left (err, attemptCount))
        Right ok -> return (Right ok)

-- |
-- Resiliently execute an action, threading a state through attempts.
retryStateful ::
  RetryStrategy ->
  -- | Initial attempt state.
  state ->
  -- | Action to execute on each attempt.
  (state -> IO (Either state ok)) ->
  IO (Either state ok)
retryStateful (RetryStrategy pauses) initialAttemptState attemptStep =
  go initialAttemptState pauses
  where
    go !attemptState retryState =
      attemptStep attemptState >>= \case
        Right ok -> return (Right ok)
        Left nextAttemptState -> case retryState of
          [] -> return (Left attemptState)
          delayInMilliseconds : nextRetryState -> do
            threadDelay $ 1000 * delayInMilliseconds
            go nextAttemptState nextRetryState
