module Coalmine.TH.Exp where

import Coalmine.InternalPrelude hiding (lift)
import Data.Attoparsec.Text qualified as Atto
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

mconcatExp :: [Exp] -> Exp
mconcatExp exps =
  AppE (VarE 'mconcat) (ListE exps)

liftPurely :: (Lift a) => a -> Exp
liftPurely =
  unsafePerformIO . runQ . lift
