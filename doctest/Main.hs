module Main where

import qualified Build_doctests as Bd
import Test.DocTest
import Prelude

main :: IO ()
main =
  doctest args
  where
    args =
      Bd.flags <> Bd.pkgs <> Bd.module_sources
