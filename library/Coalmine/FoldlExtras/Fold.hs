module Coalmine.FoldlExtras.Fold where

import Coalmine.Prelude
import Control.Foldl (Fold (..))

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
