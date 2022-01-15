module Coalmine.Name.Attoparsec where

import Coalmine.Prelude
import Data.Attoparsec.Text hiding (sepBy)
import VectorBuilder.MonadPlus

complete parser = parser <* endOfInput

nameWords = sepBy nameWord (char '-')

nameWord = takeWhile1 (\a -> isAsciiLower a || isDigit a)
