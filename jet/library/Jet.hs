{-# OPTIONS_GHC -Wno-missing-methods #-}

module Jet where

import Coalmine.Prelude
import ListT (ListT (..))

runDispatch :: Dispatch a -> IO a
runDispatch =
  error "TODO"

data Dispatch a

instance Functor Dispatch

instance Applicative Dispatch

instance Alternative Dispatch

instance Monad Dispatch

tell :: Reactor i o -> i -> Dispatch ()
tell = error "TODO"

listen :: Reactor i o -> Dispatch o
listen = error "TODO"

-- | Handler producing series of effects,
-- emitting outputs.
--
-- Runs on dedicated threads.
newtype Reactor i o
  = -- TODO: Add clean up
    Reactor (i -> ListT IO o)

startStateMachineReactor :: StateMachine i o -> IO (Reactor i o)
startStateMachineReactor = error "TODO"

startStdinReactor :: IO (Reactor Void ByteString)
startStdinReactor = error "TODO"

startKeyPressesReactor :: IO (Reactor Void Char)
startKeyPressesReactor = error "TODO"

startStdoutReactor :: IO (Reactor ByteString ())
startStdoutReactor = error "TODO"

-- | Pure state machine.
data StateMachine i o = forall state.
  StateMachine
  { start :: state,
    transition :: i -> state -> ([o], Maybe state)
  }
