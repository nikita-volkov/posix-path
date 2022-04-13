module Coalmine.FoldlExtras.Fold where

import Coalmine.InternalPrelude
import Control.Foldl (Fold (..))
import qualified Data.Machine.Mealy as Mealy
import qualified Data.Vector.Unboxed as UVec

-- |
-- Apply to unboxed vector.
foldUVec :: Unbox i => Fold i o -> UVec i -> o
foldUVec (Fold step init extract) vec =
  UVec.foldl' step init vec & extract

discretize :: Int -> Int -> (a -> Int) -> (a -> b) -> Fold b o -> Fold a o
discretize distance initEndPosition toPosition toOutput (Fold step init extract) =
  Fold step' init' extract'
  where
    init' = (initEndPosition, init)
    step' (endPosition, acc) input =
      if endPosition > position
        then error "TODO"
        else error "TODO"
      where
        position =
          toPosition input
    extract' =
      error "TODO"

applyMealy :: Mealy.Mealy i o -> Fold o r -> Fold i r
applyMealy mealy (Fold oProgress oStart oFinish) =
  Fold iProgress iStart iFinish
  where
    iProgress (oState, Mealy.Mealy runMealy) i =
      case runMealy i of
        (o, nextMealy) -> (oProgress oState o, nextMealy)
    iStart = (oStart, mealy)
    iFinish (oState, _) = oFinish oState
