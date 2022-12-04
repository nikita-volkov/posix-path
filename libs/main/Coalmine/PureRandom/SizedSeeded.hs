module Coalmine.PureRandom.SizedSeeded
  ( SizedSeeded,
    oneOf,
    vectorOf,
    seed,
    uniformElement,
  )
where

import Coalmine.InternalPrelude
import Coalmine.PureRandom.Seeded qualified as Seeded
import Data.Vector qualified as BVec
import Data.Vector.Generic qualified as GVec

newtype SizedSeeded a
  = SizedSeeded (Double -> Word -> Seeded.Seeded a)
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT Double (ReaderT Word Seeded.Seeded))

nest :: SizedSeeded a -> SizedSeeded a
nest (SizedSeeded run) = SizedSeeded $ \reductionFactor maxSize ->
  run reductionFactor (round (fromIntegral maxSize * reductionFactor))

seeded :: Seeded.Seeded a -> SizedSeeded a
seeded = SizedSeeded . const . const

vectorOf :: GVec.Vector v a => SizedSeeded a -> SizedSeeded (v a)
vectorOf (SizedSeeded runElement) = SizedSeeded $ \reductionFactor maxSize -> do
  actualSize <- Seeded.smallerSeed maxSize
  Seeded.vectorOfLength
    (fromIntegral actualSize)
    (runElement reductionFactor (round (fromIntegral maxSize * reductionFactor)))

oneOf :: BVec.Vector (SizedSeeded a) -> SizedSeeded a
oneOf vec =
  join $ uniformElement vec

seed :: SizedSeeded Word
seed = seeded Seeded.seed

uniformElement :: BVec.Vector a -> SizedSeeded a
uniformElement vec =
  seeded $ Seeded.uniformElement vec
