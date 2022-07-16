module Coalmine.TH.QuasiQuoter where

import Coalmine.InternalPrelude hiding (exp, lift)
import Coalmine.TH.Exp
import qualified Data.Attoparsec.Text as Atto
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

mapFromExp :: ((String -> Q Exp) -> (String -> Q Exp)) -> QuasiQuoter -> QuasiQuoter
mapFromExp mapper qq =
  qq {quoteExp = mapper (quoteExp qq)}

flatMapExp :: (Exp -> Q Exp) -> QuasiQuoter -> QuasiQuoter
flatMapExp mapper qq =
  qq {quoteExp = quoteExp qq >=> mapper}

exp :: (String -> Q Exp) -> QuasiQuoter
exp exp =
  QuasiQuoter
    exp
    (const (fail "Context unsupported"))
    (const (fail "Context unsupported"))
    (const (fail "Context unsupported"))

dec :: (String -> Q [Dec]) -> QuasiQuoter
dec dec =
  QuasiQuoter
    (const (fail "Context unsupported"))
    (const (fail "Context unsupported"))
    (const (fail "Context unsupported"))
    dec

pureAttoparsedExp :: Atto.Parser Exp -> QuasiQuoter
pureAttoparsedExp parser =
  exp
    (either fail pure . Atto.parseOnly parser' . fromString)
  where
    parser' = parser <* Atto.endOfInput

pureAttoparsedLiftable :: Lift a => Atto.Parser a -> QuasiQuoter
pureAttoparsedLiftable = pureAttoparsedExp . fmap liftPurely
