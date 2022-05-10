-- |
-- Collection of textual rendering classes of various formats.
module Coalmine.Printing where

import Coalmine.InternalPrelude
import qualified Coalmine.MultilineTextBuilder as Mb
import qualified Data.Text.IO as TextIO
import qualified TextBuilderDev as Ub

-- * Compact

printCompactAs :: (IsomorphicTo b TextBuilder, CompactPrinting a) => a -> b
printCompactAs = to . toCompactBuilder

printCompactAsText :: CompactPrinting a => a -> Text
printCompactAsText = printCompactAs

printCompactAsString :: CompactPrinting a => a -> String
printCompactAsString = printCompactAs

printCompactToStdOut :: CompactPrinting a => a -> IO ()
printCompactToStdOut = TextIO.putStr . printCompactAs

printLnCompactToStdOut :: CompactPrinting a => a -> IO ()
printLnCompactToStdOut = TextIO.putStrLn . printCompactAs

-- ** --

-- |
-- Textual rendering intended for logs and etc.
class CompactPrinting a where
  toCompactBuilder :: a -> TextBuilder

instance CompactPrinting Text where
  toCompactBuilder = to

instance CompactPrinting String where
  toCompactBuilder = to

instance CompactPrinting TextBuilder where
  toCompactBuilder = id

instance CompactPrinting Int where
  toCompactBuilder = Ub.decimal

instance CompactPrinting Word where
  toCompactBuilder = Ub.unsignedDecimal

-- * Pretty

printPrettyAs :: (IsomorphicTo b Mb.Builder, PrettyPrinting a) => a -> b
printPrettyAs = to . toPrettyBuilder

printPrettyAsText :: PrettyPrinting a => a -> Text
printPrettyAsText = printPrettyAs

printPrettyAsString :: PrettyPrinting a => a -> String
printPrettyAsString = printPrettyAs

printPrettyToStdOut :: PrettyPrinting a => a -> IO ()
printPrettyToStdOut = TextIO.putStr . printPrettyAs

printLnPrettyToStdOut :: PrettyPrinting a => a -> IO ()
printLnPrettyToStdOut = TextIO.putStrLn . printPrettyAs

-- ** --

class PrettyPrinting a where
  toPrettyBuilder :: a -> Mb.Builder

instance PrettyPrinting Mb.Builder where
  toPrettyBuilder = id

instance PrettyPrinting Text where
  toPrettyBuilder = to

instance PrettyPrinting String where
  toPrettyBuilder = to

instance PrettyPrinting TextBuilder where
  toPrettyBuilder = to

instance PrettyPrinting Int where
  toPrettyBuilder = toPrettyBuilder . Ub.decimal

instance PrettyPrinting Word where
  toPrettyBuilder = toPrettyBuilder . Ub.unsignedDecimal
