module TestSuites.EvenSimplerPaths where

import Coalmine.EvenSimplerPaths
import Coalmine.Prelude
import Coalmine.Tasty

tests =
  [ testCase ". is the same as empty" $
      assertEqual "" mempty $
        fromString @Path "."
  ]
