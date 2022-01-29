module Coalmine.Inter.Streamlining where

import qualified Coalmine.Inter.Structures.LiteralConstructor as LiteralConstructor
import Coalmine.Prelude
import qualified Data.Text as Text
import qualified Data.Vector as BVec

-- *

data Cmd
  = ReturnCarriageCmd !Int
  | PrintCmd !Text
  | AddPlaceholder !Text

data Symbol
  = LiteralSymbol
      !Text
  | PlaceholderSymbol
      !Int
      -- ^ Indentation.
      !Text

-- *

transduce :: [Cmd] -> [Symbol]
transduce =
  go LiteralConstructor.start
  where
    go !litCons = \case
      cmd : tail -> case cmd of
        ReturnCarriageCmd indent ->
          go (LiteralConstructor.returnCarriage indent litCons) tail
        PrintCmd text ->
          go (LiteralConstructor.addLineContent text litCons) tail
        AddPlaceholder placeholder ->
          let (indent, text) = LiteralConstructor.finish litCons
           in if Text.null text
                then
                  PlaceholderSymbol indent placeholder :
                  go LiteralConstructor.start tail
                else
                  LiteralSymbol text :
                  PlaceholderSymbol indent placeholder :
                  go LiteralConstructor.start tail
