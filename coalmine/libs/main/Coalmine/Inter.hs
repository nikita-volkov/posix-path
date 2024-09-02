-- |
-- Layout for text with indentation using efficient specialised builders.
module Coalmine.Inter where

import Coalmine.Inter.Deindentation qualified as Deindentation
import Coalmine.Inter.Syntax.Parsers qualified as Parsers
import Coalmine.Inter.TH qualified as InterTH
import Coalmine.InternalPrelude
import QqExtras qualified as QuasiQuoter

i :: QuasiQuoter
i = QuasiQuoter.pureAttoparsedExp parser
  where
    parser =
      Parsers.quasiQuote
        <&> Deindentation.quasiQuote
        <&> InterTH.fromLinesExp

j :: QuasiQuoter
j = QuasiQuoter.pureAttoparsedExp parser
  where
    parser =
      Parsers.quasiQuote
        <&> Deindentation.quasiQuote
        <&> InterTH.linesExp
