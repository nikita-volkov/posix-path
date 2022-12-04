module Coalmine.EvenSimplerPaths.IsomorphismClassHelpers where

import Coalmine.InternalPrelude
import Coalmine.Printing

thruText :: (CompactPrinting a, LenientParser b) => a -> b
thruText =
  fromRight (error "Oops! Unparsable path has crawled in")
    . parseTextLeniently
    . printCompactAsText
