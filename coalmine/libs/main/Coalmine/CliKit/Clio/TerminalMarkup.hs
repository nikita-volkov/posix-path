-- |
-- Markup renderings required by the standard Clio definitions.
module Coalmine.CliKit.Clio.TerminalMarkup where

import Coalmine.EvenSimplerPaths (Path)
import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.InternalPrelude
import Coalmine.Printing
import Coalmine.TerminalMarkup

ioError :: IOError -> TerminalMarkup
ioError =
  error "TODO"

multilineListing :: [TerminalMarkup] -> TerminalMarkup
multilineListing =
  error "TODO"

extensions :: [Text] -> TerminalMarkup
extensions =
  plainText . foldMap (mappend ".")

path :: Path -> TerminalMarkup
path =
  plainText . printCompactAs
