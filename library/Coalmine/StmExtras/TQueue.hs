module Coalmine.StmExtras.TQueue where

import Coalmine.InternalPrelude

peekWholeTQueue :: TQueue a -> STM [a]
peekWholeTQueue queue = do
  list <- flushTQueue queue
  forM_ list $ \a -> writeTQueue queue a
  return list
