module Coalmine.Lens where

import Prelude
import Control.Lens


makeSuffixedLenses = let
  mapper = \ case
    '_' : rest -> [rest ++ "Lens"]
    _ -> []
  in makeLensesWith (lensRules & lensField .~ mappingNamer mapper)
