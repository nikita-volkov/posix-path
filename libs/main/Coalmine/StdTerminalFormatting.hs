module Coalmine.StdTerminalFormatting
  ( StdTerminalFormatting (..),
    outputToStdout,
    outputToStderr,
    outputAndTerminate,
  )
where

import Coalmine.InternalPrelude
import Coalmine.TerminalMarkup qualified as TerminalMarkup

-- | Standard formatting for output to terminal (CLI).
class StdTerminalFormatting a where
  toStdTerminalMarkup :: a -> TerminalMarkup.TerminalMarkup

-- | As is.
instance StdTerminalFormatting TerminalMarkup.TerminalMarkup where
  toStdTerminalMarkup = id

-- | Render as plain text.
instance StdTerminalFormatting Text where
  toStdTerminalMarkup = TerminalMarkup.plainText

instance StdTerminalFormatting Int where
  toStdTerminalMarkup = TerminalMarkup.plainText . fromString . show

instance StdTerminalFormatting Int8 where
  toStdTerminalMarkup = TerminalMarkup.plainText . fromString . show

instance StdTerminalFormatting Int16 where
  toStdTerminalMarkup = TerminalMarkup.plainText . fromString . show

instance StdTerminalFormatting Int32 where
  toStdTerminalMarkup = TerminalMarkup.plainText . fromString . show

instance StdTerminalFormatting Int64 where
  toStdTerminalMarkup = TerminalMarkup.plainText . fromString . show

instance StdTerminalFormatting Word where
  toStdTerminalMarkup = TerminalMarkup.plainText . fromString . show

instance StdTerminalFormatting Word8 where
  toStdTerminalMarkup = TerminalMarkup.plainText . fromString . show

instance StdTerminalFormatting Word16 where
  toStdTerminalMarkup = TerminalMarkup.plainText . fromString . show

instance StdTerminalFormatting Word32 where
  toStdTerminalMarkup = TerminalMarkup.plainText . fromString . show

instance StdTerminalFormatting Word64 where
  toStdTerminalMarkup = TerminalMarkup.plainText . fromString . show

-- * Execution

outputToStdout :: StdTerminalFormatting a => a -> IO ()
outputToStdout =
  TerminalMarkup.outputToStdout . toStdTerminalMarkup

outputToStderr :: StdTerminalFormatting a => a -> IO ()
outputToStderr =
  TerminalMarkup.outputToStderr . toStdTerminalMarkup

outputAndTerminate :: StdTerminalFormatting a => a -> IO ()
outputAndTerminate =
  TerminalMarkup.outputAndTerminate . toStdTerminalMarkup
