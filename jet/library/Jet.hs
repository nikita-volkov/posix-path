{-# OPTIONS_GHC -Wno-missing-methods #-}

module Jet where

import Coalmine.Prelude
import ListT (ListT (..))

dispatch :: Source a -> Sink b -> StateMachine a b -> IO ()
dispatch source sink StateMachine {..} = go start
  where
    go !state = do
      input <- atomically source.listen
      case transition input state of
        Just (outputs, state) -> do
          atomically $ forM_ outputs sink.tell
          go state
        Nothing -> return ()

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
    transition :: i -> state -> Maybe ([o], state)
  }

instance Category StateMachine where
  id =
    StateMachine
      { start = (),
        transition = \i state -> Just ([i], state)
      }
  StateMachine leftStart leftTransition . StateMachine rightStart rightTransition =
    StateMachine
      { start = (leftStart, rightStart),
        transition = \a (leftState, rightState) -> do
          (rightOutputs, rightState) <- rightTransition a rightState
          let go !leftOutputPacks !leftState rightOutputs = case rightOutputs of
                rightOutputsHead : rightOutputsTail -> do
                  (leftOutputs, leftState) <- leftTransition rightOutputsHead leftState
                  go (leftOutputs : leftOutputPacks) leftState rightOutputsTail
                _ ->
                  pure (mconcat (reverse leftOutputPacks), (leftState, rightState))
           in go [] leftState rightOutputs
      }
