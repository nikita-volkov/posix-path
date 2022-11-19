module TestSuites.Inter where

import Coalmine.Inter
import Coalmine.Prelude
import Data.Text qualified as Text
import Test.QuickCheck qualified as QuickCheck
import Test.QuickCheck.Instances
import Test.QuickCheck.Property qualified as QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import TestSuites.Inter.Regressions.FromFormatica qualified as RegressionsFromFormatica

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
