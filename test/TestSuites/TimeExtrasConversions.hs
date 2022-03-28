module TestSuites.TimeExtrasConversions where

import Coalmine.Prelude
import Coalmine.Tasty
import Coalmine.TimeExtras.Conversions

tests =
  [ testProperty "UTCTime-Millis Iso" $ \millis ->
      millis === (utcTimeMillisSinceEpoch . millisSinceEpochUTCTime) millis
  ]
