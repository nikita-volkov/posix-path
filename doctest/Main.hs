module Main where

import Prelude
import Test.DocTest
import qualified Build_doctests as Bd


main :: IO ()
main =
  doctest args
  where
    args =
      Bd.flags <> Bd.pkgs <> Bd.module_sources
