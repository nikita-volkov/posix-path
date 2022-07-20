module Coalmine.Tasty
  ( -- * Reexports from the original Tasty modules.
    module Test.Tasty,
    module Test.Tasty.HUnit,
    module Test.Tasty.QuickCheck,

    -- * Extensions

    -- ** Main
    suitesMain,

    -- ** Test trees
    module Coalmine.Tasty.TestTrees,
  )
where

import Coalmine.InternalPrelude
import Coalmine.Tasty.TestTrees
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- | Individually initialized test trees.
--
-- Helpful for uniting multiple modules.
suitesMain :: [IO TestTree] -> IO ()
suitesMain suites =
  defaultMain . testGroup "Suites"
    =<< sequence suites
