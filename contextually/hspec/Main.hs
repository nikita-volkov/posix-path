module Main where

import Contextually
import Test.Hspec
import Prelude

main :: IO ()
main = hspec do
  describe "" do
    it "" do
      res <- try @SomeException $ contextually "a" do
        contextually "b" do
          fail "c"
      case res of
        Left exception -> shouldBe (displayException exception) "a/b: user error (c)"
        Right () -> return ()
