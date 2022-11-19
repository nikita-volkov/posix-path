module Coalmine.Tasty.TestTrees.Cereal where

import Coalmine.CerealExtras.Get qualified as Get
import Coalmine.InternalPrelude
import Data.Serialize qualified as Cereal
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

testEncodeDecode :: (Arbitrary a, Eq a, Show a, Cereal.Serialize a) => Proxy a -> TestTree
testEncodeDecode proxy =
  testProperty "Encode-decode equals original" $ \a ->
    Right (asProxyTypeOf a proxy) === Get.runCompletely Cereal.get (Cereal.encode a)
