module Coalmine.ConduitExtras where

import Coalmine.InternalPrelude hiding (yield)
import Conduit
import qualified Control.Foldl as Foldl
import qualified Data.Machine.Mealy as MachinesMealy
import qualified Data.Machine.Moore as MachinesMoore

-- * --

foldSink :: Monad m => Foldl.Fold i r -> ConduitT i o m r
foldSink (Foldl.Fold progress start finish) =
  go start
  where
    go !state =
      await >>= \case
        Just i -> go (progress state i)
        Nothing -> pure $ finish state

mooreSink :: Monad m => MachinesMoore.Moore i r -> ConduitT i o m r
mooreSink (MachinesMoore.Moore terminate next) =
  await >>= \case
    Just i -> mooreSink $ next i
    Nothing -> pure terminate

moorePipe :: Monad m => MachinesMoore.Moore i o -> ConduitT i o m ()
moorePipe (MachinesMoore.Moore emit next) =
  yield emit >> await >>= \case
    Just i -> moorePipe $ next i
    Nothing -> pure ()

mealyPipe :: Monad m => MachinesMealy.Mealy i o -> ConduitT i o m ()
mealyPipe (MachinesMealy.Mealy run) =
  await >>= \case
    Just i -> case run i of
      (o, next) -> yield o >> mealyPipe next
    Nothing -> pure ()

-- * --

discretize :: (Monad m) => Int -> (i -> Int) -> (Int -> i -> o) -> ConduitT i o m ()
discretize distance toPosition toOutput =
  await >>= \case
    Just sample -> go (toPosition sample + distance) sample (toPosition sample) sample
    Nothing -> return ()
  where
    go boundaryPosition lastSample position sample =
      if position < boundaryPosition
        then
          await >>= \case
            Just newSample -> go boundaryPosition sample (toPosition newSample) newSample
            Nothing -> yield (toOutput boundaryPosition sample)
        else
          yield (toOutput boundaryPosition lastSample)
            >> go (boundaryPosition + distance) lastSample position sample

droppingPipe :: Monad m => Int -> ConduitT a a m ()
droppingPipe amount =
  dropC amount >> transmit

-- | Identity conduit.
transmit :: Monad m => ConduitT a a m ()
transmit =
  mapC id

-- |
-- Skip every N inputs.
--
-- Useful for downsampling.
skipEvery :: Monad m => Int -> ConduitT a a m ()
skipEvery amount =
  if amount <= 0
    then mapC id
    else go amount
  where
    go !currentAmount =
      await >>= \case
        Nothing -> return ()
        Just i ->
          if currentAmount == 0
            then yield i >> go amount
            else go (pred currentAmount)
