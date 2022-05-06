-- |
-- Collection of textual rendering classes of various formats.
module Coalmine.Printing where

import Coalmine.InternalPrelude
import qualified Coalmine.MultilineTextBuilder as Mb
import Coalmine.StringIsomorphism
import Coalmine.TextIsomorphism
import qualified Data.Text.IO as TextIO
import qualified TextBuilderDev as Ub

-- * Compact

printCompactAsText :: CompactPrinting a => a -> Text
printCompactAsText =
  toText . toCompactBuilder

printCompactAsString :: CompactPrinting a => a -> String
printCompactAsString =
  toString . toCompactBuilder

printCompactToStdOut :: CompactPrinting a => a -> IO ()
printCompactToStdOut =
  TextIO.putStr . printCompactAsText

printLnCompactToStdOut :: CompactPrinting a => a -> IO ()
printLnCompactToStdOut =
  TextIO.putStrLn . printCompactAsText

-- ** --

-- |
-- Textual rendering intended for logs and etc.
class CompactPrinting a where
  toCompactBuilder :: a -> TextBuilder

instance CompactPrinting Text where
  toCompactBuilder = fromText

instance CompactPrinting String where
  toCompactBuilder = fromString

instance CompactPrinting TextBuilder where
  toCompactBuilder = fromTextBuilder

instance CompactPrinting Int where
  toCompactBuilder = Ub.decimal

instance CompactPrinting Word where
  toCompactBuilder = Ub.unsignedDecimal

-- * Pretty

printPrettyAsText :: PrettyPrinting a => a -> Text
printPrettyAsText =
  toText . toPrettyBuilder

printPrettyAsString :: PrettyPrinting a => a -> String
printPrettyAsString =
  toString . toPrettyBuilder

printPrettyToStdOut :: PrettyPrinting a => a -> IO ()
printPrettyToStdOut =
  TextIO.putStr . printPrettyAsText

printLnPrettyToStdOut :: PrettyPrinting a => a -> IO ()
printLnPrettyToStdOut =
  TextIO.putStrLn . printPrettyAsText

-- ** --

class PrettyPrinting a where
  toPrettyBuilder :: a -> Mb.Builder

instance PrettyPrinting Mb.Builder where
  toPrettyBuilder = id

instance PrettyPrinting Text where
  toPrettyBuilder = fromText

instance PrettyPrinting String where
  toPrettyBuilder = fromString

instance PrettyPrinting TextBuilder where
  toPrettyBuilder = fromTextBuilder

instance PrettyPrinting Int where
  toPrettyBuilder = toPrettyBuilder . Ub.decimal

instance PrettyPrinting Word where
  toPrettyBuilder = toPrettyBuilder . Ub.unsignedDecimal
