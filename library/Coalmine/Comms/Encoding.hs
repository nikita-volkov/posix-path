module Coalmine.Comms.Encoding where

import Coalmine.Comms.StreamingWrite qualified as StreamingWrite
import Coalmine.InternalPrelude

data Encoding = Encoding
  { size :: Int,
    write :: StreamingWrite.StreamingWrite
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
  Encoding 0 $ StreamingWrite.failure reason
