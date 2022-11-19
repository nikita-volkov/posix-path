module Main where

import Build_doctests qualified as Bd
import Coalmine.InternalPrelude
import Test.DocTest

main :: IO ()
main =
  doctest args
  where
    args =
      Bd.flags <> Bd.pkgs <> Bd.module_sources
