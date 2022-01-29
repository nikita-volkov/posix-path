module Coalmine.Inter.Structures.LiteralConstructor where

import qualified Coalmine.Inter.Normalization as N
import qualified Coalmine.MultilineTextBuilder as B
import Coalmine.Prelude
import Language.Haskell.TH.Syntax
import qualified THLego.Helpers as Helpers

-- *

data LiteralConstructor
  = LiteralConstructor
      !Int
      -- ^ Current line local indent.
      ![Text]
      -- ^ Current line chunks.
      ![Text]
      -- ^ Preceding lines.

-- *

start :: Int -> LiteralConstructor
start = error "TODO"

finish :: LiteralConstructor -> Text
finish (LiteralConstructor indent chunks lines) =
  error "TODO"

-- *

returnCarriage :: Int -> LiteralConstructor -> LiteralConstructor
returnCarriage indent = error "TODO"
