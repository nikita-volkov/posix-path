{-# OPTIONS_GHC -Wno-missing-methods -Wno-unused-imports #-}

module Jet.ReactorResearchLib where

import Coalmine.Prelude
import Data.ByteString qualified as ByteString
import ListT (ListT (..))
import ListT qualified

-- | Pure state machine.
data Machine i o = forall state.
  Machine
  { start :: state,
    transition :: i -> state -> Maybe ([o], state)
  }

instance Category Machine where
  id =
    Machine
      { start = (),
        transition = \i state -> Just ([i], state)
      }
  Machine leftStart leftTransition . Machine rightStart rightTransition =
    Machine
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

-- dispatch :: Machine a b -> Reactor b a -> IO ()
-- dispatch _ _ =
--   error "TODO"

-- data Reactor a b
--   = Reactor {
--     -- tell :: a -> IO (),
--     -- listen :: IO a
--     -- run :: Machine
--     -- iterate :: (b -> IO ()) -> a -> IO ()
--     iterate :: a -> ListT IO b
--   }

-- prepatch ::

-----

-- dispatch :: Machine a b -> ( b -> ListT IO a) -> IO ()
-- dispatch _ _ =
--   error "TODO"

-- No. This will only cause generation after input

-- dispatch :: Machine a b -> ListT IO a -> ( b -> ListT IO a) -> IO ()
-- dispatch _ _ =
--   error "TODO"

-- startStdin :: IO (ListT IO ByteString)
-- startStdin =
--   error "TODO"

-- startStdout :: IO (ByteString -> IO ())

----

-- data Reactor a b
--   = forall state.
--   Reactor {
--     start :: state,
--     transition :: state -> a -> (ListT IO b, state)
--   }

-- data Reactor a b
--   = Reactor {
--     iterate :: a -> IO ([b], Reactor a b)
--   }

data Reactor a b = Reactor
  { listen :: STM (Maybe b),
    -- subscribe :: IO (IO (Maybe b)),
    tell :: a -> STM (),
    stop :: IO ()
  }

dispatch :: Machine a b -> Reactor b a -> IO ()
dispatch Machine {..} Reactor {..} =
  go start
  where
    go !state = do
      a <- atomically listen
      case a of
        Just a -> case transition a state of
          Just (bs, state) -> do
            atomically $ forM_ bs tell
            go state
          Nothing -> stop
        Nothing -> stop

startStdin :: IO (Reactor Void ByteString)
startStdin =
  error "TODO"

startStdout :: IO (Reactor ByteString Void)
startStdout =
  error "TODO"
