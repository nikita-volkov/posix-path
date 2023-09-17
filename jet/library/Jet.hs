{-# OPTIONS_GHC -Wno-missing-methods #-}

module Jet where

import Coalmine.Prelude

runDispatch :: Dispatch a -> IO a
runDispatch (Dispatch run) =
  go
  where
    go =
      atomically run >>= maybe go return

newtype Dispatch a
  = -- | Result of nothing means termination.
    Dispatch (STM (Maybe a))

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
data Reactor i o =
  -- TODO: Add clean up
  Reactor
  { tell :: i -> STM (),
    listen :: STM [o]
  }

startStateMachineReactor :: StateMachine i o -> IO (Reactor i o)
startStateMachineReactor (StateMachine start transition) = do
  inQueue <- newTBQueueIO 100
  outQueue <- newTBQueueIO 100
  forkIO $ do
    let fetch !state = do
          inputs <- atomically $ flushTBQueue inQueue
          processInputs state inputs
        processInputs !state inputs = do
          case inputs of
            head : tail ->
              case transition head state of
                (outputs, state) -> do
                  atomically $ forM_ outputs $ writeTBQueue outQueue
                  case state of
                    Nothing -> return ()
                    Just state -> processInputs state tail
            _ ->
              fetch state
     in fetch start
  return
    Reactor
      { tell = writeTBQueue inQueue,
        listen = flushTBQueue outQueue
      }

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
