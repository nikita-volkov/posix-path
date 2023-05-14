module Coalmine.BaseExtras.Natural where

import Coalmine.InternalPrelude

parseIntegral :: (Integral a) => a -> Maybe Natural
parseIntegral a =
  if a < 0
    then Nothing
    else Just $ fromIntegral a
