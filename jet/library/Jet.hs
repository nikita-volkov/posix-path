{-# OPTIONS_GHC -Wno-missing-methods #-}

module Jet where

import Coalmine.Prelude
import Data.ByteString qualified as ByteString
import ListT (ListT (..))
import ListT qualified

dispatch :: Source a -> Sink b -> StateMachine a b -> IO ()
dispatch source sink _ =
  error "TODO"

-- | Output port of a stream producer.
data Source a = Source
  { listen :: STM (Maybe a),
    stop :: STM ()
  }

instance Functor Source

instance Applicative Source

instance Alternative Source where
  empty =
    Source
      { listen = pure Nothing,
        stop = pure ()
      }
  left <|> right =
    Source
      { listen = left.listen <|> right.listen,
        stop = left.stop *> right.stop
      }

instance Monad Source

-- | Input port of a stream consumer.
data Sink a = Sink
  { tell :: a -> STM (),
    stop :: STM ()
  }

instance Semigroup (Sink a)

instance Monoid (Sink a)

instance Contravariant Sink

instance Divisible Sink

instance Decidable Sink

startStdin :: IO (Source ByteString)
startStdin = error "TODO"

startKeyPresses :: IO (Source Char)
startKeyPresses = error "TODO"

startStdout :: IO (Sink ByteString)
startStdout = do
  inputChannel <- newTBQueueIO 100
  forkIO $
    let go = do
          chunk <- atomically $ readTBQueue inputChannel
          case chunk of
            Just chunk -> do
              ByteString.hPut stdout chunk
              go
            Nothing -> return ()
     in go
  return
    Sink
      { tell = writeTBQueue inputChannel . Just,
        stop = writeTBQueue inputChannel Nothing
      }

startReactor :: (i -> ListT IO o) -> IO (Sink i, Source o)
startReactor reactor = do
  inputChannel <- newTBQueueIO 100
  outputChannel <- newTBQueueIO 100
  forkIO $
    let go = do
          input <- atomically $ readTBQueue inputChannel
          forM_ input $ \input -> do
            ListT.traverse_ (atomically . writeTBQueue outputChannel . error "TODO") (reactor input)
            go
     in go
  let tell = writeTBQueue inputChannel . Just
      stop = writeTBQueue inputChannel Nothing
      listen = readTBQueue outputChannel

  return (Sink {..}, Source {..})

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
