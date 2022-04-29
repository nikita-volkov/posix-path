module Coalmine.TH.QuasiQuoter where

import Coalmine.InternalPrelude
import qualified Data.Attoparsec.Text as Atto
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

mapFromExp :: ((String -> Q Exp) -> (String -> Q Exp)) -> QuasiQuoter -> QuasiQuoter
mapFromExp mapper qq =
  qq {quoteExp = mapper (quoteExp qq)}

flatMapExp :: (Exp -> Q Exp) -> QuasiQuoter -> QuasiQuoter
flatMapExp mapper qq =
  qq {quoteExp = quoteExp qq >=> mapper}

fromExp :: (String -> Q Exp) -> QuasiQuoter
fromExp fromExp =
  QuasiQuoter
    fromExp
    (const (fail "Context unsupported"))
    (const (fail "Context unsupported"))
    (const (fail "Context unsupported"))

pureAttoparsedExp :: Atto.Parser Exp -> QuasiQuoter
pureAttoparsedExp parser =
  fromExp
    (either fail pure . Atto.parseOnly parser' . fromString)
  where
    parser' = parser <* Atto.endOfInput
