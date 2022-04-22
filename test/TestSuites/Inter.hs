module TestSuites.Inter where

import Coalmine.Inter
import Coalmine.Prelude
import qualified Data.Text as Text
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import qualified Test.QuickCheck.Property as QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import qualified TestSuites.Inter.Regressions.FromFormatica as RegressionsFromFormatica

tests =
  [ testCase "" $
      let expected = "a:\n  - a\n  b\n  c.\n\n  - b:\n    a\n    b\n    c\n  - c: $"
          actual :: Text
          actual =
            [i|
              a:
                - $var.

                - b:
                  ${var}
                - c: $$
            |]
            where
              var :: Text
              var = "a\nb\nc"
       in assertEqual "" expected actual,
    testCase "Single-line parses fine" $
      assertEqual "" " a " ([i| a |] :: Text),
    testGroup "Regressions" $
      [ testGroup "From Formatica" RegressionsFromFormatica.tests
      ]
  ]
