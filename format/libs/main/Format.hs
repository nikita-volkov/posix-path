module Format where

import Data.Attoparsec.Text qualified as Attoparsec
import TextBuilderDev
import Prelude hiding (print)

class Format format where
  type FormatData format
  formatPrinter :: format -> FormatData format -> TextBuilder
  formatParser :: format -> Attoparsec.Parser (FormatData format)
