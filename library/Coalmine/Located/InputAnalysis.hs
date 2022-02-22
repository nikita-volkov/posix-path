-- |
-- Search by lines and etc.
module Coalmine.Located.InputAnalysis where

import Coalmine.Located.InputAnalysis.Model
import Coalmine.Prelude
import qualified Data.Text as Text
import Optics

-- *

updateIterator :: Char -> Iterator -> Iterator
updateIterator char iterator =
  iterator
    & over #lineConstructor (updateLineConstructor (iterator ^. #offset) char)
    & over #offset succ
    & error "TODO"

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

-- *

finalizeLineConstructor :: Text -> LineConstructor -> (Int, Text)
finalizeLineConstructor input = \case
  FinishedLineConstructor constructor ->
    let text =
          input
            & Text.drop (constructor ^. #start)
            & Text.take (constructor ^. #end - constructor ^. #start)
        lineNum =
          constructor ^. #line
     in (lineNum, text)
