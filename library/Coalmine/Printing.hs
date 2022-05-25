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

-- * Broad

printBroadAs :: (IsomorphicTo b Mb.Builder, BroadPrinting a) => a -> b
printBroadAs = to . toBroadBuilder

printBroadAsText :: BroadPrinting a => a -> Text
printBroadAsText = printBroadAs

printBroadAsString :: BroadPrinting a => a -> String
printBroadAsString = printBroadAs

printBroadToStdOut :: BroadPrinting a => a -> IO ()
printBroadToStdOut = TextIO.putStr . printBroadAs

printLnBroadToStdOut :: BroadPrinting a => a -> IO ()
printLnBroadToStdOut = TextIO.putStrLn . printBroadAs

-- ** --

class BroadPrinting a where
  toBroadBuilder :: a -> Mb.Builder

instance BroadPrinting Mb.Builder where
  toBroadBuilder = id

instance BroadPrinting Text where
  toBroadBuilder = to

instance BroadPrinting String where
  toBroadBuilder = to

instance BroadPrinting TextBuilder where
  toBroadBuilder = to

instance BroadPrinting Int where
  toBroadBuilder = toBroadBuilder . Ub.decimal

instance BroadPrinting Word where
  toBroadBuilder = toBroadBuilder . Ub.unsignedDecimal
