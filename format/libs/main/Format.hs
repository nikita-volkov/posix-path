module Format where

import Data.Attoparsec.Text qualified as Attoparsec
import Data.Text (Text)
import Data.Text qualified as Text
import TextBuilderDev (TextBuilder)
import TextBuilderDev qualified
import Prelude hiding (print)

printWithFormat :: (Format f a) => f -> a -> Text
printWithFormat format value =
  TextBuilderDev.buildText (formatPrinter format value)

parseWithFormat :: (Format f a) => f -> Text -> Either Text a
parseWithFormat format input =
  case Attoparsec.parseOnly parser input of
    Right value -> Right value
    Left errorString -> Left (Text.pack errorString)
  where
    parser =
      formatParser format <* Attoparsec.endOfInput

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
