-- |
-- Search by lines and etc.
module Coalmine.Located.InputAnalysis where

import Coalmine.Located.InputAnalysis.Model
import Coalmine.Prelude
import qualified Data.Text as Text
import Optics

-- *

updateIterator :: Char -> Iterator -> Iterator
updateIterator char =
  error "TODO"

updateLineConstructor :: Int -> Char -> LineConstructor -> LineConstructor
updateLineConstructor offset char = \case
  CollectingLineConstructor constructor ->
    if char == '\n' || char == '\r'
      then
        FinishedLineConstructor $
          LineConstructorFinished
            (constructor ^. #line)
            (constructor ^. #start)
            offset
      else CollectingLineConstructor constructor
  FinishedLineConstructor constructor ->
    FinishedLineConstructor constructor
