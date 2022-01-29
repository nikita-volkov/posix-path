module Coalmine.Inter.Streamlining where

import qualified Coalmine.Inter.Format.Model as M
import qualified Coalmine.Inter.Normalization as N
import Coalmine.Prelude
import qualified Data.Vector as BVec

-- *

data Symbol
  = LiteralSymbol !Text
  | PlaceholderSymbol
      !Int
      -- ^ Indentation.
      !Text

-- *
