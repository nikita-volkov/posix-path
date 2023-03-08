module TestSuites.LeanPaths where

import Coalmine.LeanPaths.NormalizedPath
import Coalmine.Prelude hiding (Path)
import Coalmine.SyntaxModellingLaws qualified as SyntaxModellingLaws
import Coalmine.Tasty
import Coalmine.Tasty.TestTrees.Cereal qualified as Cereal

tests =
  [ Cereal.testEncodeDecode @NormalizedPath Proxy,
    testProperties "Syntax" $ SyntaxModellingLaws.properties $ Proxy @NormalizedPath,
    testGroup "Empty" $
      [ eqTestCase @NormalizedPath
          "equals empty"
          mempty
          "",
        eqTestCase @NormalizedPath
          "equals dot"
          "."
          ""
      ],
    testGroup "Dot" $
      [ eqTestCase @NormalizedPath
          "equals empty"
          mempty
          ".",
        eqTestCase @NormalizedPath
          "acts as a normal component"
          "src/main"
          "src/./main"
      ],
    testGroup "Trailing slash" $
      [ eqTestCase @NormalizedPath
          "Same as without it"
          "src/main/java"
          "src/main/java/"
      ],
    testGroup "Multislash" $
      [ eqTestCase @NormalizedPath
          "Double"
          "src/main"
          "src//main",
        eqTestCase @NormalizedPath
          "Triple"
          "src/main"
          "src///main"
      ]
  ]
