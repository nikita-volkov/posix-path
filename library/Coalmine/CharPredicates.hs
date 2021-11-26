-- |
-- Indentation-sensitive line-by-line parsing.
module Coalmine.CharPredicates where

import Coalmine.Prelude

-- | Is it a horizontal space character?
isHSpace :: Char -> Bool
isHSpace x = isSpace x && x /= '\n' && x /= '\r'

isNotNewline :: Char -> Bool
isNotNewline x = x /= '\n' && x /= '\r'

isNewline :: Char -> Bool
isNewline x = x == '\n' || x == '\r'

isAsciiAlpha :: Char -> Bool
isAsciiAlpha x = isAscii x && isAlpha x

-- * Combinators

either :: (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
either l r x = l x || r x

both :: (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
both l r x = l x && r x
