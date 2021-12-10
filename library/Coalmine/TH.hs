module Coalmine.TH where

import Coalmine.Prelude
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

pureExpQq :: (String -> Exp) -> QuasiQuoter
pureExpQq exp =
  QuasiQuoter
    (pure . exp)
    (const (fail "Context unsupported"))
    (const (fail "Context unsupported"))
    (const (fail "Context unsupported"))
