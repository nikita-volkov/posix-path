module Coalmine.Isomorphisms.Update where

import Coalmine.Prelude
import Data.Isomorphism

type UpdateIso a = Iso (->) a a

addInt :: Int -> UpdateIso Int
addInt x = Iso (+ x) (subtract x)

subtractInt :: Int -> UpdateIso Int
subtractInt x = Iso (subtract x) (+ x)

updateMap :: k -> UpdateIso v -> UpdateIso (Map k v)
updateMap = error "TODO"
