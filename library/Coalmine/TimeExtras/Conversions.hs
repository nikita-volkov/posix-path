module Coalmine.TimeExtras.Conversions where

import Coalmine.InternalPrelude
import qualified Data.Time.Clock.System as Time

millisecondsSinceEpochUTCTime :: Integer -> UTCTime
millisecondsSinceEpochUTCTime milliseconds =
  let (day, dayMilliseconds) = divMod milliseconds 86400000
   in UTCTime (sinceEpochDay day) (millisecondsDiffTime dayMilliseconds)

sinceEpochDay :: Integer -> Day
sinceEpochDay =
  flip addDays systemEpochDay

millisecondsDiffTime :: Integer -> DiffTime
millisecondsDiffTime =
  picosecondsToDiffTime . (* 1000000000)

diffTimeMilliseconds :: DiffTime -> Integer
diffTimeMilliseconds =
  flip div 1000000000 . diffTimeToPicoseconds

daySinceEpoch :: Day -> Integer
daySinceEpoch =
  flip diffDays systemEpochDay

dayMillisecondsSinceEpoch :: Day -> Integer
dayMillisecondsSinceEpoch =
  (* 86400000) . daySinceEpoch

utcTimeMillisecondsSinceEpoch :: UTCTime -> Integer
utcTimeMillisecondsSinceEpoch (UTCTime day diffTime) =
  dayMillisecondsSinceEpoch day + diffTimeMilliseconds diffTime
