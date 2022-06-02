-- |
-- Reexports for the tasty modules.
module Coalmine.Tasty
  ( module Exports,
    eqTestCase,
  )
where

import Coalmine.InternalPrelude
import Test.QuickCheck.Instances as Exports
import Test.Tasty as Exports
import Test.Tasty.HUnit as Exports
import Test.Tasty.QuickCheck as Exports

eqTestCase :: (Eq a, Show a) => TestName -> a -> a -> TestTree
eqTestCase name expected actual =
  testCase name $ assertEqual "" expected actual
