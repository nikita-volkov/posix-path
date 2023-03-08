module TestSuites.LeanPaths where

import Coalmine.LeanPaths.NormalizedPath
import Coalmine.Prelude hiding (Path)
import Coalmine.SyntaxModellingLaws qualified as SyntaxModellingLaws
import Coalmine.Tasty
import Coalmine.Tasty.TestTrees.Cereal qualified as Cereal
import Test.QuickCheck.Classes qualified as QuickcheckClasses

tests =
  [ testGroup "NormalizedPath" $
      [ Cereal.testEncodeDecode @NormalizedPath Proxy,
        testProperties "Syntax" $ SyntaxModellingLaws.properties $ Proxy @NormalizedPath,
        testLaws (QuickcheckClasses.monoidLaws (Proxy @NormalizedPath)),
        testGroup "Essentials" $
          [ eqTestCase
              "Component decomposition works and keeps order"
              ["src", "main", "java"]
              (decompose "src/main/java")
          ],
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
              "src/main/java/",
            eqTestCase
              "Doesn't produce a trailing component"
              ["src", "main", "java"]
              (decompose "src/main/java/")
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
          ],
        testGroup "toFilePath" $
          [ eqTestCase
              "Empty renders as dot"
              "."
              (toFilePath mempty),
            eqTestCase
              "Relative is prefixed with dot"
              "./a"
              (toFilePath "a"),
            eqTestCase
              "Absolute is prefixed with slash"
              "/a"
              (toFilePath (root <> "a"))
          ],
        testGroup "Dot-dot" $
          [ eqTestCase @NormalizedPath
              "Intermediate ones get squashed"
              "a/c/f"
              "a/b/../c/d/e/../../f",
            eqTestCase @NormalizedPath
              "Concatenation"
              "a/d"
              ("a/b/c" <> "../../d")
          ]
      ]
  ]
