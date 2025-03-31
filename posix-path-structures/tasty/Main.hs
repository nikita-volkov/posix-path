module Main where

import Coalmine.Prelude hiding (Path)
import Coalmine.SyntaxModellingQuickCheck qualified as SyntaxModellingQuickCheck
import Coalmine.Tasty
import Coalmine.Tasty.TestTrees.Cereal qualified as Cereal
import PosixPathStructures.NormalizedPath
import Test.QuickCheck.Classes qualified as QuickcheckClasses

main :: IO ()
main =
  defaultMain
    . testGroup "PosixPathStructures"
    $ [ testGroup "NormalizedPath"
          $ [ Cereal.testEncodeDecode @NormalizedPath Proxy,
              testProperties "Syntax" $ SyntaxModellingQuickCheck.all $ Proxy @NormalizedPath,
              testLaws (QuickcheckClasses.monoidLaws (Proxy @NormalizedPath)),
              testGroup "Essentials"
                $ [ eqTestCase
                      "Component decomposition works and keeps order"
                      ["src", "main", "java"]
                      (decompose "src/main/java")
                  ],
              testGroup "Empty"
                $ [ eqTestCase @NormalizedPath
                      "equals empty"
                      mempty
                      "",
                    eqTestCase @NormalizedPath
                      "equals dot"
                      "."
                      ""
                  ],
              testGroup "Dot"
                $ [ eqTestCase @NormalizedPath
                      "equals empty"
                      mempty
                      ".",
                    eqTestCase @NormalizedPath
                      "acts as a normal component"
                      "src/main"
                      "src/./main"
                  ],
              testGroup "Trailing slash"
                $ [ eqTestCase @NormalizedPath
                      "Same as without it"
                      "src/main/java"
                      "src/main/java/",
                    eqTestCase
                      "Doesn't produce a trailing component"
                      ["src", "main", "java"]
                      (decompose "src/main/java/")
                  ],
              testGroup "Multislash"
                $ [ eqTestCase @NormalizedPath
                      "Double"
                      "src/main"
                      "src//main",
                    eqTestCase @NormalizedPath
                      "Triple"
                      "src/main"
                      "src///main"
                  ],
              testGroup "toFilePath"
                $ [ eqTestCase
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
              testGroup "fromString"
                $ [ eqTestCase @NormalizedPath
                      "Intermediate ones get squashed"
                      "a/c/f"
                      "a/b/../c/d/e/../../f",
                    eqTestCase @NormalizedPath
                      "Absolute followed by dot-dot"
                      "/a"
                      "/../a"
                  ],
              testGroup "mappend"
                $ [ eqTestCase @NormalizedPath
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
                  ],
              testGroup "toText"
                $ [ eqTestCase "Trailing slash" "./a" (toText "a/"),
                    eqTestCase "Multislash" "./a/b" (toText "a//b"),
                    eqTestCase "Empty" "." (toText ""),
                    eqTestCase "Move up" ".." (toText "..")
                  ],
              testGroup "relativeTo"
                $ [ eqTestCase "" (Just "..") (relativeTo "a/b" "a/b/c"),
                    eqTestCase "" (Just "../b") (relativeTo "a/b" "a/c"),
                    eqTestCase "" (Just "../a") (relativeTo "a" "b"),
                    eqTestCase "" (Just "..") (relativeTo "." "b"),
                    eqTestCase "" (Just "a") (relativeTo "a" "."),
                    eqTestCase "" (Just "/a") (relativeTo "/a" "b"),
                    eqTestCase "" Nothing (relativeTo "a" "/b"),
                    eqTestCase "" Nothing (relativeTo "a" "..")
                  ]
            ]
      ]
