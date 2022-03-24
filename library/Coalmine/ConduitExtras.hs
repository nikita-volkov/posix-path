module Coalmine.ConduitExtras where

import Coalmine.Prelude
import Conduit
import qualified Control.Foldl as Foldl
import qualified Data.Machine.Moore as MachinesMoore

-- *

fold :: Monad m => Foldl.Fold i r -> Conduit.ConduitT i o m r
fold (Foldl.Fold progress start finish) =
  go start
  where
    go !state =
      await >>= \case
        Just i -> go (progress state i)
        Nothing -> pure $ finish state

moore :: Monad m => MachinesMoore.Moore i r -> Conduit.ConduitT i o m r
moore (MachinesMoore.Moore terminate next) =
  await >>= \case
    Just i -> moore $ next i
    Nothing -> pure terminate
