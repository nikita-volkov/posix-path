{-# OPTIONS_GHC -Wno-missing-methods #-}

module Jet where

import Coalmine.Prelude
import ListT (ListT (..))

dispatch :: Source a -> Sink b -> StateMachine a b -> IO ()
dispatch gen actor StateMachine {..} = go start
  where
    go !state = do
      input <- atomically gen.listen
      case transition input state of
        (outputs, state) -> do
          atomically $ forM_ outputs actor.tell
          forM_ state go

data Source a = Source
  { listen :: STM a
  }

instance Functor Source

instance Applicative Source

instance Alternative Source

instance Monad Source

data Sink a = Sink
  { tell :: a -> STM ()
  }

instance Semigroup (Sink a)

instance Monoid (Sink a)

startStdin :: IO (Source ByteString)
startStdin = error "TODO"

startKeyPresses :: IO (Source Char)
startKeyPresses = error "TODO"

startStdout :: IO (Sink ByteString)
startStdout = error "TODO"

startReactor :: (i -> ListT IO o) -> IO (Sink i, Source o)
startReactor =
  error "TODO"

-- | Pure state machine.
data StateMachine i o = forall state.
  StateMachine
  { start :: state,
    transition :: i -> state -> ([o], Maybe state)
  }
