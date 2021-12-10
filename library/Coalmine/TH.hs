module Coalmine.TH where

import Coalmine.Prelude
import qualified Data.Attoparsec.Text as Atto
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

pureAttoparsedExpQq :: Atto.Parser Exp -> QuasiQuoter
pureAttoparsedExpQq parser =
  QuasiQuoter
    (either fail pure . Atto.parseOnly parser . fromString)
    (const (fail "Context unsupported"))
    (const (fail "Context unsupported"))
    (const (fail "Context unsupported"))
