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

diffTimeMillis :: DiffTime -> Int
diffTimeMillis =
  fromIntegral . flip div 1000000000 . diffTimeToPicoseconds

daySinceEpoch :: Day -> Int
daySinceEpoch =
  fromIntegral . flip diffDays systemEpochDay

dayMillisSinceEpoch :: Day -> Int
dayMillisSinceEpoch =
  (* 86400000) . daySinceEpoch

utcTimeMillisSinceEpoch :: UTCTime -> Int
utcTimeMillisSinceEpoch (UTCTime day diffTime) =
  dayMillisSinceEpoch day + diffTimeMillis diffTime
