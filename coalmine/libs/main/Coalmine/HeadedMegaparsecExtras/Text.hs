module Coalmine.HeadedMegaparsecExtras.Text where

import Coalmine.InternalPrelude hiding (bit, filter, head, some, sortBy, tail, try)
import HeadedMegaparsec
import Text.Megaparsec.Char qualified as MegaparsecChar

-- * --

type Parser e = HeadedParsec e Text

eol :: (Ord e) => Parser e Text
eol = parse MegaparsecChar.eol
