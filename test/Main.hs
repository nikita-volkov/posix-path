module Main where

import Coalmine.Tasty
import qualified TestSuites.Inter as Inter
import qualified TestSuites.LocatedRendering as LocatedRendering
import Prelude

main =
  defaultMain . testGroup "All" $
    [ testGroup "Inter" Inter.tests,
      testGroup "LocatedRendering" LocatedRendering.tests
    ]
