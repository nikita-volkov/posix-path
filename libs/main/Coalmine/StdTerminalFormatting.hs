module Coalmine.StdTerminalFormatting
  ( StdTerminalFormatting (..),
    TerminalMarkup.outputToStdout,
    TerminalMarkup.outputToStderr,
    TerminalMarkup.outputAndTerminate,
  )
where

import Coalmine.InternalPrelude
import Coalmine.TerminalMarkup qualified as TerminalMarkup

class StdTerminalFormatting a where
  toStdTerminalMarkup :: a -> TerminalMarkup.TerminalMarkup

-- | As is.
instance StdTerminalFormatting TerminalMarkup.TerminalMarkup where
  toStdTerminalMarkup = id

-- | Render as plain text.
instance StdTerminalFormatting Text where
  toStdTerminalMarkup = TerminalMarkup.plainText
