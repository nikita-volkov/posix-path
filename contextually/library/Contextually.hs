module Contextually where

import Contextually.Handles.ThreadStates qualified as ThreadStates
import Prelude

data Contextualized a = Contextualized
  { contexts :: [Text],
    payload :: a
  }

{-# NOINLINE globalThreadStates #-}
globalThreadStates :: ThreadStates.Handle
globalThreadStates = unsafePerformIO $ ThreadStates.acquire

contextually :: Text -> IO a -> IO a
contextually context action = do
  threadId <- myThreadId
  ThreadStates.enterContext globalThreadStates threadId context
  try @SomeException action >>= \case
    Right result -> do
      ThreadStates.exitContext globalThreadStates threadId
      return result
    Left exception -> do
      error "TODO"

getContexts :: IO [Text]
getContexts =
  error "TODO"
