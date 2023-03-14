module TestSuites.Inter where

import Coalmine.Inter
import Coalmine.Prelude
import Coalmine.Tasty
import TestSuites.Inter.Regressions.FromFormatica qualified as RegressionsFromFormatica

tests :: [TestTree]
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
