module Coalmine.StmExtras.TBQueue where

import Coalmine.InternalPrelude

flushNonEmptyTBQueue :: TBQueue a -> STM (NonEmpty a)
flushNonEmptyTBQueue queue = do
  head <- readTBQueue queue
  tail <- flushTBQueue queue
  pure (head :| tail)
