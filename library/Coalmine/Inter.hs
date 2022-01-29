module Coalmine.Inter where

import qualified Coalmine.Inter.Deindentation as Deindentation
import qualified Coalmine.Inter.Format.Parsers as Parsers
import qualified Coalmine.Inter.TH as InterTH
import Coalmine.Prelude
import qualified Coalmine.TH as TH

inter :: QuasiQuoter
inter = TH.pureAttoparsedExpQq parser
  where
    parser =
      Parsers.quasiQuote
        <&> InterTH.linesExp . Deindentation.quasiQuote
