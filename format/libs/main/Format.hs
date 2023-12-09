module Format where

import Data.Attoparsec.Text qualified as Attoparsec
import TextBuilderDev
import Prelude hiding (print)

class Format format value where
  formatPrinter :: format -> value -> TextBuilder
  formatParser :: format -> Attoparsec.Parser value

data DecimalFormat = DecimalFormat
  { scientificNotation :: Bool
  }

instance Format DecimalFormat Double where
  formatPrinter =
    error "TODO"
  formatParser =
    error "TODO"
