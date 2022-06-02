module TestSuites.EvenSimplerPaths where

import Coalmine.EvenSimplerPaths
import Coalmine.Prelude
import Coalmine.Tasty

tests =
  [ testCase ". is the same as empty" $
      assertEqual "" mempty $
        fromString @Path ".",
    testCase "Trailing slash" $
      assertEqual "" (Right "src/main/java") $
        parseTextLeniently @Path "src/main/java/"
  ]
