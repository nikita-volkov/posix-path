module Coalmine.Megaparsec where

import Coalmine.InternalPrelude
import Text.Megaparsec

refineText :: Parsec Void Text a -> Text -> Either Text a
refineText p = left (fromString . errorBundlePretty) . runParser (p <* eof) ""
