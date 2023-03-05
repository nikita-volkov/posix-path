module Coalmine.Format where

import Coalmine.InternalPrelude
import Data.Attoparsec.Text qualified as Attoparsec

-- |
-- Value which can be parsed and rendered from a single format.
-- 
-- The law is that formatting a value and parsing
-- should produce the original value.
class Format a where
  parser :: Attoparsec.Parser a
  render :: a -> TextBuilder

validateLaws :: (Format a, Eq a) => a -> Bool
validateLaws val =
  case Attoparsec.parseOnly (formatParser <* Attoparsec.endOfInput) (to @Text (formatAsTextBuilder val)) of
    Right parsedVal -> parsedVal == val
    Left _ -> False
