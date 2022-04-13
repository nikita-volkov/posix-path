module Coalmine.CharPredicates where

import Coalmine.InternalPrelude hiding (both, either)
import Coalmine.Predicates

isSpaceOrTab :: Char -> Bool
isSpaceOrTab = either (== ' ') (== '\t')

-- | Is it a horizontal space character?
isHSpace :: Char -> Bool
isHSpace x = isSpace x && x /= '\n' && x /= '\r'

isNotNewline :: Char -> Bool
isNotNewline x = x /= '\n' && x /= '\r'

isNewline :: Char -> Bool
isNewline x = x == '\n' || x == '\r'

isAsciiAlpha :: Char -> Bool
isAsciiAlpha x = isAscii x && isAlpha x
