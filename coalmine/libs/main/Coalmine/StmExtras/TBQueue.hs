module Coalmine.StmExtras.TBQueue where

import Coalmine.InternalPrelude

flushNonEmptyTBQueue :: TBQueue a -> STM (NonEmpty a)
flushNonEmptyTBQueue x = do
  head <- readTBQueue x
  tail <- flushTBQueue x
  return (head :| tail)

-- | Get a list of all entries in the queue without removing them.
inspectTBQueue :: TBQueue a -> STM [a]
inspectTBQueue queue = do
  list <- flushTBQueue queue
  forM_ list $ writeTBQueue queue
  return list
