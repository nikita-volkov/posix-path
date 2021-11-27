module Coalmine.Isomorphisms where

import Coalmine.Prelude
import Data.Isomorphism
import qualified Data.Map.Strict as Map

-- *

data PartialIso a b
  = PartialIso
      (a -> Maybe b)
      (b -> Maybe a)

instance Semigroupoid PartialIso where
  o (PartialIso li lo) (PartialIso ri ro) =
    PartialIso
      (ri >=> li)
      (lo >=> ro)

instance Groupoid PartialIso where
  inv (PartialIso i o) = PartialIso o i

instance Category PartialIso where
  id = PartialIso pure pure
  (.) = o

-- *

newtype PartialEndoIso a
  = PartialEndoIso (PartialIso a a)

instance Invariant PartialEndoIso where
  invmap = error "TODO"

instance Semigroup (PartialEndoIso a) where
  PartialEndoIso l <> PartialEndoIso r =
    PartialEndoIso (o l r)

instance Monoid (PartialEndoIso a) where
  mempty = PartialEndoIso id

instance Group (PartialEndoIso a) where
  invert = coerce (inv :: PartialIso a a -> PartialIso a a)

monoPartial :: (a -> a) -> (a -> a) -> PartialEndoIso a
monoPartial forw back =
  PartialEndoIso $ PartialIso (Just . forw) (Just . back)

isoMonoPartial :: Iso (->) a a -> PartialEndoIso a
isoMonoPartial = error "TODO"

addInt :: Int -> PartialEndoIso Int
addInt x = monoPartial (+ x) (subtract x)

subtractInt :: Int -> PartialEndoIso Int
subtractInt x = monoPartial (subtract x) (+ x)

atMapKey :: k -> PartialEndoIso v -> PartialEndoIso (Map k v)
atMapKey = error "TODO"
