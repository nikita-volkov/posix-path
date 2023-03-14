module TestSuites.TimeExtrasConversions where

import Coalmine.Prelude
import Coalmine.Tasty
import Coalmine.TimeExtras.Conversions

tests :: [TestTree]
tests =
  [ testProperty "UTCTime-Millis Iso" $ \millis ->
      millis === (utcTimeMillisecondsSinceEpoch . millisecondsSinceEpochUTCTime) millis
  ]
