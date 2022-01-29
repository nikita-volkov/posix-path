module Coalmine.Inter.Streamlining where

import qualified Coalmine.Inter.Normalization as N
import qualified Coalmine.Inter.Structures.LiteralConstructor as LiteralConstructor
import Coalmine.Prelude
import qualified Data.Vector as BVec

-- *

data Symbol
  = LiteralSymbol !Text
  | PlaceholderSymbol
      !Int
      -- ^ Indentation.
      !Text

-- *

newtype Streamliner
  = Streamliner LiteralConstructor.LiteralConstructor

returnCarriage :: Int -> Streamliner -> Streamliner
returnCarriage = error "TODO"

print :: Text -> Streamliner -> Streamliner
print = error "TODO"

addPlaceholder :: Text -> Streamliner -> ([Symbol], Streamliner)
addPlaceholder = error "TODO"
