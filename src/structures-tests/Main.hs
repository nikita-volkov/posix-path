module Main where

import PosixPath.Structures.NormalizedPath
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Classes qualified
import Prelude

main :: IO ()
main = hspec do
  describe "PosixPath.Structures" do
    describe "NormalizedPath" do
      describe "Laws" do
        itFollowsLaws (Test.QuickCheck.Classes.semigroupLaws (Proxy @NormalizedPath))
        itFollowsLaws (Test.QuickCheck.Classes.monoidLaws (Proxy @NormalizedPath))

      describe "Essentials" do
        itEquals
          "Component decomposition works and keeps order"
          ["src", "main", "java"]
          (toComponents "src/main/java")

      describe "Empty" do
        itEquals @NormalizedPath
          "equals empty"
          mempty
          ""
        itEquals @NormalizedPath
          "equals dot"
          "."
          ""

      describe "Dot" do
        itEquals @NormalizedPath
          "equals empty"
          mempty
          "."
        itEquals @NormalizedPath
          "acts as a normal component"
          "src/main"
          "src/./main"

      describe "Trailing slash" do
        itEquals @NormalizedPath
          "Same as without it"
          "src/main/java"
          "src/main/java/"
        itEquals
          "Doesn't produce a trailing component"
          ["src", "main", "java"]
          (toComponents "src/main/java/")

      describe "Multislash" do
        itEquals @NormalizedPath
          "Double"
          "src/main"
          "src//main"
        itEquals @NormalizedPath
          "Triple"
          "src/main"
          "src///main"

      describe "toFilePath" do
        itEquals
          "Empty renders as dot"
          "."
          (toFilePath mempty)
        itEquals
          "Relative is prefixed with dot"
          "./a"
          (toFilePath "a")
        itEquals
          "Absolute is prefixed with slash"
          "/a"
          (toFilePath "/a")

      describe "fromString" do
        itEquals @NormalizedPath
          "Intermediate ones get squashed"
          "a/c/f"
          "a/b/../c/d/e/../../f"
        itEquals @NormalizedPath
          "Absolute followed by dot-dot"
          "/a"
          "/../a"

      describe "mappend" do
        itEquals @NormalizedPath
          "Dot-dot"
          "a/d"
          ("a/b/c" <> "../../d")
        itEquals @NormalizedPath
          "Second absolute"
          "/d"
          ("a/b/c" <> "/d")
        itEquals @NormalizedPath
          "First absolute and dot-dot too high"
          "/a"
          ("/" <> "../a")
        itEquals @NormalizedPath
          "Absolute is prefixed with slash"
          "/a"
          (root <> "a")

      describe "toText" do
        itEquals "Trailing slash" "./a" (toText "a/")
        itEquals "Multislash" "./a/b" (toText "a//b")
        itEquals "Empty" "." (toText "")
        itEquals "Move up" ".." (toText "..")

      describe "relativeTo" do
        itEquals "" (Just "..") (relativeTo "a/b" "a/b/c")
        itEquals "" (Just "../b") (relativeTo "a/b" "a/c")
        itEquals "" (Just "../a") (relativeTo "a" "b")
        itEquals "" (Just "..") (relativeTo "." "b")
        itEquals "" (Just "a") (relativeTo "a" ".")
        itEquals "" (Just "/a") (relativeTo "/a" "b")
        itEquals "" Nothing (relativeTo "a" "/b")
        itEquals "" Nothing (relativeTo "a" "..")

itEquals :: (Show a, Eq a) => String -> a -> a -> Spec
itEquals name a b =
  it name (shouldBe a b)

itFollowsLaws :: (HasCallStack) => Test.QuickCheck.Classes.Laws -> Spec
itFollowsLaws laws =
  describe laws.lawsTypeclass do
    forM_ laws.lawsProperties (uncurry prop)
