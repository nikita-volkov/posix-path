module Coalmine.Tasty
  ( -- * Reexports of the original Tasty modules.
    module Exports,

    -- * Extensions
    suitesMain,
  )
where

import Coalmine.InternalPrelude
import Coalmine.Tasty.TestTrees as Exports
import Test.QuickCheck.Instances as Exports
import Test.Tasty as Exports
import Test.Tasty.HUnit as Exports
import Test.Tasty.QuickCheck as Exports

-- | Individually initialized test trees.
--
-- Helpful for uniting multiple modules.
suitesMain :: [IO TestTree] -> IO ()
suitesMain suites =
  defaultMain . testGroup "Suites"
    =<< sequence suites
