module Coalmine.SimplePaths.AttoparsecHelpers where

import Coalmine.Prelude
import Data.Attoparsec.Text

complete parser = parser <* endOfInput

directories = sepBy directory (char '/')

directory = takeWhile1 (\a -> a /= '/' && a /= '\\')
