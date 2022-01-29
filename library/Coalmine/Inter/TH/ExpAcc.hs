-- |
-- In this module we're taking a perspective of
-- a cursor sequentially going thru the quoted string.
--
-- Since the contexts of where the cursor is positioned change,
-- we represent it with types. Thus getting the lifecycle typed.
module Coalmine.Inter.TH.ExpAcc where

import qualified Coalmine.Inter.Normalization as N
import qualified Coalmine.MultilineTextBuilder as B
import Coalmine.Prelude
import Language.Haskell.TH.Syntax
import qualified THLego.Helpers as Helpers

-- *

data BuildingLiteral
  = BuildingLiteral
      !Int
      -- ^ Current line local indent.
      ![Text]
      -- ^ Current line chunks.
      ![Text]
      -- ^ Preceding lines.
      ![Exp]
      -- ^ Preceding expressions

data BuildingExps
  = BuildingExps
      !Int
      -- ^ Current line local indent.
      ![Exp]

-- *

returnCarriage :: Int -> BuildingLiteral -> BuildingLiteral
returnCarriage indent = error "TODO"

startAddingLiteral :: BuildingExps -> BuildingLiteral
startAddingLiteral = error "TODO"

-- |
-- Finish and commit the accumulated literal.
finishAddingLiteral :: BuildingLiteral -> BuildingExps
finishAddingLiteral (BuildingLiteral indent chunks lines exps) =
  error "TODO"
  where
    exp = error "TODO"

addExp :: Exp -> BuildingExps -> BuildingExps
addExp = error "TODO"

-- *

data Symbol
  = LiteralSymbol !Text
  | PlaceholderSymbol
      !Int
      -- ^ Indentation.
      !Text
