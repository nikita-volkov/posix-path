module Coalmine.Comms.Encoding where

import Coalmine.InternalPrelude
import Coalmine.PtrKit.Streamer qualified as Streamer

data Encoding = Encoding
  { size :: Int,
    write :: Streamer.Streamer
  }

instance Semigroup Encoding where
  Encoding leftSize leftWrite <> Encoding rightSize rightWrite =
    Encoding (leftSize + rightSize) (leftWrite <> rightWrite)

instance Monoid Encoding where
  mempty = Encoding 0 mempty

varLengthInteger :: Integer -> Encoding
varLengthInteger =
  error "TODO"

failure :: Text -> Encoding
failure reason =
  Encoding 0 $ Streamer.failure reason
