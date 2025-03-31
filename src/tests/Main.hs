module Main where

import PosixPath
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Classes qualified
import Prelude

main :: IO ()
main = hspec do
  describe "Laws" do
    itFollowsLaws (Test.QuickCheck.Classes.semigroupLaws (Proxy @Path))
    itFollowsLaws (Test.QuickCheck.Classes.monoidLaws (Proxy @Path))

  describe "Essentials" do
    itEquals
      "Component decomposition works and keeps order"
      ["src", "main", "java"]
      (toSegments "src/main/java")

  describe "Empty" do
    itEquals @Path
      "equals empty"
      mempty
      ""
    itEquals @Path
      "equals dot"
      "."
      ""

  describe "Dot" do
    itEquals @Path
      "equals empty"
      mempty
      "."
    itEquals @Path
      "acts as a normal component"
      "src/main"
      "src/./main"

  describe "Trailing slash" do
    itEquals @Path
      "Same as without it"
      "src/main/java"
      "src/main/java/"
    itEquals
      "Doesn't produce a trailing segment"
      ["src", "main", "java"]
      (toSegments "src/main/java/")

  describe "Multislash" do
    itEquals @Path
      "Double"
      "src/main"
      "src//main"
    itEquals @Path
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
    itEquals @Path
      "Intermediate ones get squashed"
      "a/c/f"
      "a/b/../c/d/e/../../f"
    itEquals @Path
      "Absolute followed by dot-dot"
      "/a"
      "/../a"

  describe "mappend" do
    itEquals @Path
      "Dot-dot"
      "a/d"
      ("a/b/c" <> "../../d")
    itEquals @Path
      "Second absolute"
      "/d"
      ("a/b/c" <> "/d")
    itEquals @Path
      "First absolute and dot-dot too high"
      "/a"
      ("/" <> "../a")
    itEquals @Path
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
