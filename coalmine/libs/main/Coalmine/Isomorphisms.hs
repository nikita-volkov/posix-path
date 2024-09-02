module Coalmine.Isomorphisms where

import Coalmine.InternalPrelude
import Data.Isomorphism
import Data.Map.Strict qualified as Map

-- * --

type EndoPartialIso a = PartialIso a a

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

-- ** --

totalPartialIso :: (a -> b) -> (b -> a) -> PartialIso a b
totalPartialIso forw back =
  PartialIso (Just . forw) (Just . back)

isoPartialIso :: Iso (->) a b -> PartialIso a b
isoPartialIso = error "TODO"

addInt :: Int -> EndoPartialIso Int
addInt x = totalPartialIso (+ x) (subtract x)

subtractInt :: Int -> EndoPartialIso Int
subtractInt x = totalPartialIso (subtract x) (+ x)

-- |
-- Update an element in map if it exists.
--
-- The whole operation fails if it doesn't.
updateMap :: (Ord k) => k -> EndoPartialIso v -> EndoPartialIso (Map k v)
updateMap k (PartialIso vi _vo) =
  PartialIso mi mo
  where
    mi = Map.alterF alter k
      where
        alter = \case
          Nothing -> Nothing
          Just a -> case vi a of
            Nothing -> Nothing
            Just a -> Just (Just a)
    mo = Map.alterF alter k
      where
        alter = \case
          Nothing -> Nothing
          Just a -> case vi a of
            Nothing -> Nothing
            Just a -> Just (Just a)

-- |
-- Insert only if the key is not present,
-- failing otherwise
insertInMap :: k -> v -> EndoPartialIso (Map k v)
insertInMap = error "TODO"

ensureMapContainsKey :: k -> EndoPartialIso (Map k v)
ensureMapContainsKey _k = error "TODO"
