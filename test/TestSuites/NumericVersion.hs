module TestSuites.NumericVersion where

import Coalmine.NumericVersion
import Coalmine.Prelude
import Coalmine.Tasty

tests =
  [ eqTestCase "Bumps existing places" [lit|1.2|] (bump 1 [lit|1.1|]),
    eqTestCase "Creates missing places" [lit|1.0.1|] (bump 2 [lit|1|])
  ]
