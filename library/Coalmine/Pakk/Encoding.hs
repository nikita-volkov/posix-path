module Coalmine.Pakk.Encoding where

import Coalmine.InternalPrelude
import Coalmine.Pakk.Write qualified as Write

data Encoding = Encoding
  { size :: Int,
    write :: Write.Write
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
  Encoding 0 $ Write.failure reason
