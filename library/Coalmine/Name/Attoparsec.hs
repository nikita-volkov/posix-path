module Coalmine.Name.Attoparsec where

import Coalmine.InternalPrelude
import Data.Attoparsec.Text hiding (sepBy, sepBy1)
import VectorBuilder.MonadPlus

complete parser = parser <* endOfInput

nameWords = sepBy1 nameWord (char '-')

nameWord = takeWhile1 (\a -> isAsciiLower a || isDigit a)
