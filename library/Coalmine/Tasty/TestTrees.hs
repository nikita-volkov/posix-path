module Coalmine.Tasty.TestTrees where

import Coalmine.InternalPrelude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

eqTestCase :: (Eq a, Show a) => TestName -> a -> a -> TestTree
eqTestCase name expected actual =
  testCase name $ assertEqual "" expected actual

eitherEqTestCase :: (Eq a, Show a) => TestName -> a -> Either Text a -> TestTree
eitherEqTestCase name expected actual =
  testCase name $ case actual of
    Right actual -> assertEqual "" expected actual
    Left err -> assertFailure $ to err
