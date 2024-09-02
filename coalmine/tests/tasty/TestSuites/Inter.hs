{-# LANGUAGE OverloadedRecordDot #-}

module TestSuites.Inter where

import Coalmine.Inter
import Coalmine.Prelude
import Coalmine.Tasty
import TestSuites.Inter.Regressions.FromFormatica qualified as RegressionsFromFormatica

tests :: [TestTree]
tests =
  [ testGroup "Expectations"
      $ [ testCase "Indents and handles dollar"
            $ let expected =
                    "a:\n\
                    \  - a\n\
                    \  b\n\
                    \  c.\n\
                    \\n\
                    \  - b:\n\
                    \    a\n\
                    \    b\n\
                    \    c\n\
                    \  - c: $\n\
                    \  - d:"
                  actual :: Text
                  actual =
                    [i|
                      a:
                        - ${var}.

                        - b:
                          ${var}
                        - c: $
                        - d:
                    |]
                    where
                      var :: Text
                      var = "a\nb\nc"
               in assertEqual "" expected actual,
          testCase "Single-line parses fine"
            $ assertEqual "" " a " ([i| a |] :: Text),
          testCase "Record-dot"
            $ assertEqual "" ("a" :: Text)
            $ let record = RecordDotExample {field = "a"}
               in [i|${record.field}|]
        ],
    testGroup "Regressions"
      $ [ testGroup "From Formatica" RegressionsFromFormatica.tests
        ]
  ]

data RecordDotExample = RecordDotExample
  {field :: Text}
