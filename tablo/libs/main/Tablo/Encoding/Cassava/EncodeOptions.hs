-- | Encoding options presets.
module Tablo.Encoding.Cassava.EncodeOptions where

import Data.Char
import Data.Csv
import Prelude

tsv :: Bool -> EncodeOptions
tsv includeHeader =
  EncodeOptions
    { encUseCrLf = True,
      encQuoting = QuoteNone,
      encIncludeHeader = includeHeader,
      encDelimiter = fromIntegral (ord '\t')
    }
