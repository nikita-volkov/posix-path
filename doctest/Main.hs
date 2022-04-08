module Main where

import qualified Build_doctests as Bd
import Coalmine.InternalPrelude
import Test.DocTest

main :: IO ()
main =
  doctest args
  where
    args =
      Bd.flags <> Bd.pkgs <> Bd.module_sources
