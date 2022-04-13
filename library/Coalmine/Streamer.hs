module Coalmine.Streamer where

import Coalmine.InternalPrelude hiding (State)
import qualified Jsonifier

-- |
-- Specification of how to stream values of a particular type.
newtype Streamer a
  = Streamer (a -> DataStreamBuilder)

-- |
-- Efficiently constructs a lazy list that can then be used
-- as a generator of data.
buildDataStream :: DataStreamBuilder -> [ByteString]
buildDataStream = error "TODO"

newtype DataStreamBuilder
  = DataStreamBuilder (Conf -> State -> Maybe (ByteString, State))

-- |
-- Configuration of how to stream.
-- Affects the state update logic.
data Conf
  = -- | Chunk size.
    Conf
      !Int

data State
