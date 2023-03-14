module Coalmine.Tasty.TestTrees where

import Coalmine.InternalPrelude
import Test.QuickCheck.Classes qualified as QuickcheckClasses
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

eitherIOEqTestCase :: (Eq a, Show a) => TestName -> a -> IO (Either Text a) -> TestTree
eitherIOEqTestCase name expected getActual =
  testCase name $
    getActual >>= \case
      Right actual -> assertEqual "" expected actual
      Left err -> assertFailure $ to err

testLaws :: QuickcheckClasses.Laws -> TestTree
testLaws laws =
  testProperties (QuickcheckClasses.lawsTypeclass laws) (QuickcheckClasses.lawsProperties laws)
