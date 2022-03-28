module Coalmine.TimeExtras.Conversions where

import Coalmine.InternalPrelude
import qualified Data.Time.Clock.System as Time

millisSinceEpochUTCTime :: Int -> UTCTime
millisSinceEpochUTCTime millis =
  let (day, dayMillis) = divMod millis 86400000
   in UTCTime (sinceEpochDay day) (millisDiffTime dayMillis)

sinceEpochDay :: Int -> Day
sinceEpochDay =
  flip addDays systemEpochDay . fromIntegral

millisDiffTime :: Int -> DiffTime
millisDiffTime =
  picosecondsToDiffTime . (* 1000000000) . fromIntegral
