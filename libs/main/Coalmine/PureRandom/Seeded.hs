module Coalmine.PureRandom.Seeded where

import Coalmine.InternalPrelude
import Data.Vector qualified as BVec
import Data.Vector.Generic qualified as GVec

-- |
-- General randomizer, which can be easily adapted to all standard randomization libs
-- like \"QuickGen\" or \"random\".
newtype Seeded a
  = Seeded ((Word -> Word) -> Word -> (a, Word))
  deriving (Functor)

instance Applicative Seeded where
  pure =
    error "TODO"
  Seeded runL <*> Seeded runR =
    Seeded $ \iterate seed ->
      case runL iterate seed of
        (resL, seedL) -> case runR iterate seedL of
          (resR, seedR) -> (resL resR, seedR)

instance Monad Seeded where
  return = pure
  Seeded runL >>= cont =
    Seeded $ \iterate seed ->
      case runL iterate seed of
        (resL, seedL) -> case cont resL of
          Seeded runR -> runR iterate seedL

seed :: Seeded Word
seed = Seeded $ \iterate seed -> (seed, iterate seed)

smallerSeed :: Word -> Seeded Word
smallerSeed maxSeed =
  seed <&> \seed -> rem seed maxSeed

oneOf :: BVec.Vector (Seeded a) -> Seeded a
oneOf vec =
  join $ uniformElement vec

vectorOfLength :: GVec.Vector v a => Int -> Seeded a -> Seeded (v a)
vectorOfLength length =
  error "TODO"

uniformElement :: BVec.Vector a -> Seeded a
uniformElement vec =
  smallerSeed (fromIntegral (BVec.length vec)) <&> BVec.unsafeIndex vec . fromIntegral
