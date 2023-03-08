module TestSuites.PosixPath where

import Coalmine.PosixPath.NormalizedPath
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
              (toFilePath "/a")
          ],
        testGroup "fromString" $
          [ eqTestCase @NormalizedPath
              "Intermediate ones get squashed"
              "a/c/f"
              "a/b/../c/d/e/../../f",
            eqTestCase @NormalizedPath
              "Absolute followed by dot-dot"
              "/a"
              "/../a"
          ],
        testGroup "mappend" $
          [ eqTestCase @NormalizedPath
              "Dot-dot"
              "a/d"
              ("a/b/c" <> "../../d"),
            eqTestCase @NormalizedPath
              "Second absolute"
              "/d"
              ("a/b/c" <> "/d"),
            eqTestCase @NormalizedPath
              "First absolute and dot-dot too high"
              "/a"
              ("/" <> "../a"),
            eqTestCase @NormalizedPath
              "Absolute is prefixed with slash"
              "/a"
              (root <> "a")
          ]
      ]
  ]
