module Coalmine.TimeExtras.Conversions where

import Coalmine.InternalPrelude

millisecondsSinceEpochUTCTime :: Integer -> UTCTime
millisecondsSinceEpochUTCTime time =
  let (day, dayTime) = divMod time 86400000
   in UTCTime (sinceEpochDay day) (millisecondsDiffTime dayTime)

picosecondsSinceEpochUTCTime :: Integer -> UTCTime
picosecondsSinceEpochUTCTime time =
  let (day, dayTime) = divMod time 86400000000000000
   in UTCTime (sinceEpochDay day) (picosecondsToDiffTime dayTime)

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

-- | Extract flat components of UTC
utcTimeComponents :: UTCTime -> (Integer, Int, Int, Int, Int, Pico)
utcTimeComponents (UTCTime day dayTime) =
  let (year, month, monthDay) = toGregorian day
      TimeOfDay hour min sec = timeToTimeOfDay dayTime
   in (year, month, monthDay, hour, min, sec)

-- * SystemTime

{-# NOINLINE [1] systemTimeMillisSinceEpoch #-}
systemTimeMillisSinceEpoch :: (Num value) => SystemTime -> value
systemTimeMillisSinceEpoch (MkSystemTime s ns) =
  fromIntegral (div ns 1000000) + fromIntegral (s * 1000)

{-# RULES
"systemTimeMillisSinceEpoch/Int64"
  systemTimeMillisSinceEpoch =
    systemTimeInt64MillisSinceEpoch
  #-}

systemTimeInt64MillisSinceEpoch :: SystemTime -> Int64
systemTimeInt64MillisSinceEpoch (MkSystemTime s ns) =
  fromIntegral (div ns 1000000) + s * 1000

{-# RULES
"systemTimeMicrosSinceEpoch/Int64"
  systemTimeMicrosSinceEpoch =
    systemTimeInt64MicrosSinceEpoch
  #-}

{-# NOINLINE [1] systemTimeMicrosSinceEpoch #-}
systemTimeMicrosSinceEpoch :: (Num value) => SystemTime -> value
systemTimeMicrosSinceEpoch (MkSystemTime s ns) =
  fromIntegral (div ns 1000) + fromIntegral (s * 1000000)

systemTimeInt64MicrosSinceEpoch :: SystemTime -> Int64
systemTimeInt64MicrosSinceEpoch (MkSystemTime s ns) =
  fromIntegral (div ns 1000) + s * 1000000
