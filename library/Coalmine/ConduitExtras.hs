module Coalmine.ConduitExtras where

import Coalmine.Prelude hiding (yield)
import Conduit
import qualified Control.Foldl as Foldl
import qualified Data.Machine.Moore as MachinesMoore

-- *

fold :: Monad m => Foldl.Fold i r -> ConduitT i o m r
fold (Foldl.Fold progress start finish) =
  go start
  where
    go !state =
      await >>= \case
        Just i -> go (progress state i)
        Nothing -> pure $ finish state

reducingMoore :: Monad m => MachinesMoore.Moore i r -> ConduitT i o m r
reducingMoore (MachinesMoore.Moore terminate next) =
  await >>= \case
    Just i -> reducingMoore $ next i
    Nothing -> pure terminate

mappingMoore :: Monad m => MachinesMoore.Moore i o -> ConduitT i o m ()
mappingMoore (MachinesMoore.Moore emit next) =
  yield emit >> await >>= \case
    Just i -> mappingMoore $ next i
    Nothing -> pure ()
