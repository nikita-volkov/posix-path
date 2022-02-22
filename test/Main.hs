module Main where

import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import qualified Test.QuickCheck.Property as QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import qualified TestSuites.Inter as Inter
import Prelude

main =
  defaultMain . testGroup "All" $
    [testGroup "Inter" Inter.tests]
