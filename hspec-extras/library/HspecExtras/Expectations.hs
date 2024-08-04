module HspecExtras.Expectations where

import Coalmine.Prelude
import Test.Hspec

shouldBeRight :: (Show a1, Show a2, Eq a2) => Either a1 a2 -> a2 -> Expectation
shouldBeRight actual expected = case actual of
  Left err -> expectationFailure (show err)
  Right res -> shouldBe res expected
