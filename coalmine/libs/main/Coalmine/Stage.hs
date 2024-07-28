module Coalmine.Stage where

import Coalmine.InternalPrelude

-- |
-- Useful for forcing a structure of programs that is easier to comprehend.
-- It requires the user to decompose his computations as a tree, thus removing the interdependencies in the resulting components.
-- Reduction of dependencies is the key to reduction of complexity.
data Stage error a = forall error collected.
  Stage
  { collect :: IO (Either error collected),
    postProcess :: collected -> Either error a
  }
