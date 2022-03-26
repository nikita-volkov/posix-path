module Main where

import Coalmine.Tasty
import qualified TestSuites.ConduitExtras as ConduitExtras
import qualified TestSuites.Inter as Inter
import qualified TestSuites.LocatedRendering as LocatedRendering
import qualified TestSuites.MegaparsecExtras as MegaparsecExtras
import Prelude

main =
  defaultMain . testGroup "All" $
    [ testGroup "Inter" Inter.tests,
      testGroup "LocatedRendering" LocatedRendering.tests,
      testGroup "MegaparsecExtras" MegaparsecExtras.tests,
      testGroup "ConduitExtras" ConduitExtras.tests
    ]
