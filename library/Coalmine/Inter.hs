-- |
-- Layout for text with indentation using efficient specialised builders.
module Coalmine.Inter where

import qualified Coalmine.Inter.Deindentation as Deindentation
import qualified Coalmine.Inter.Format.Parsers as Parsers
import qualified Coalmine.Inter.TH as InterTH
import Coalmine.InternalPrelude
import qualified Coalmine.TH.QuasiQuoter as QuasiQuoter

i :: QuasiQuoter
i = QuasiQuoter.pureAttoparsedExp parser
  where
    parser =
      Parsers.quasiQuote
        <&> InterTH.fromLinesExp . Deindentation.quasiQuote

i' :: QuasiQuoter
i' = QuasiQuoter.pureAttoparsedExp parser
  where
    parser =
      Parsers.quasiQuote
        <&> InterTH.linesExp . Deindentation.quasiQuote
