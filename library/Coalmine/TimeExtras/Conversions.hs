module Coalmine.TimeExtras.Conversions where

import Coalmine.InternalPrelude
import qualified Data.Time.Clock.System as Time

millisSinceEpochUTCTime :: Integer -> UTCTime
millisSinceEpochUTCTime millis =
  let (day, dayMillis) = divMod millis 86400000
   in UTCTime (sinceEpochDay day) (millisDiffTime dayMillis)

sinceEpochDay :: Integer -> Day
sinceEpochDay =
  flip addDays systemEpochDay

millisDiffTime :: Integer -> DiffTime
millisDiffTime =
  picosecondsToDiffTime . (* 1000000000)

diffTimeMillis :: DiffTime -> Integer
diffTimeMillis =
  flip div 1000000000 . diffTimeToPicoseconds

daySinceEpoch :: Day -> Integer
daySinceEpoch =
  flip diffDays systemEpochDay

dayMillisSinceEpoch :: Day -> Integer
dayMillisSinceEpoch =
  (* 86400000) . daySinceEpoch

utcTimeMillisSinceEpoch :: UTCTime -> Integer
utcTimeMillisSinceEpoch (UTCTime day diffTime) =
  dayMillisSinceEpoch day + diffTimeMillis diffTime
