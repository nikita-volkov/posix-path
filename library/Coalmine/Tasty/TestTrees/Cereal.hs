module Coalmine.Tasty.TestTrees.Cereal where

import Coalmine.InternalPrelude
import qualified Data.Serialize as Cereal
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

testEncodeDecode :: (Arbitrary a, Eq a, Show a, Cereal.Serialize a) => Proxy a -> TestTree
testEncodeDecode proxy =
  testProperty "Encode-decode equals original" $ \a ->
    Right (asProxyTypeOf a proxy) === Cereal.decode (Cereal.encode a)
