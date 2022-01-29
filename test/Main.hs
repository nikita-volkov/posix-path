module Main where

import qualified Coalmine.Test.Inter as Inter
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import qualified Test.QuickCheck.Property as QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Prelude

main =
  defaultMain . testGroup "All" $
    [testGroup "Inter" Inter.tests]
