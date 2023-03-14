-- |
-- DSL for construction of formatted output in terminal,
-- when the terminal supports it.
module Coalmine.CliKit.Echo where

import Coalmine.TerminalMarkup qualified as TerminalMarkup

newtype Echo = Echo TerminalMarkup.TerminalMarkup
