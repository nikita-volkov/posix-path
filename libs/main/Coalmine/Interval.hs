module Coalmine.Interval where

import Coalmine.InternalPrelude
import Coalmine.TimeExtras.Conversions qualified as TimeConversions
import Data.Ratio ((%))

-- * --

newtype Interval = Interval Integer
  deriving (Num, Real, Ord, Eq)

instance LenientParser Interval where
  lenientParser =
    Interval . diffTimeToPicoseconds
      <$> lenientParser

instance Show Interval where
  showsPrec _ (Interval i) =
    (showFFloat @Double Nothing . realToFrac . (% 1000000000000)) i . showChar 's'

days :: Integer -> Interval
days = Interval . (*) 86400000000000000

hours :: Integer -> Interval
hours = Interval . (*) 3600000000000000

minutes :: Integer -> Interval
minutes = Interval . (*) 60000000000000

seconds :: Integer -> Interval
seconds = Interval . (*) 1000000000000

milliseconds :: Integer -> Interval
milliseconds = Interval . (*) 1000000000

microseconds :: Integer -> Interval
microseconds = Interval . (*) 1000000

nanoseconds :: Integer -> Interval
nanoseconds = Interval . (*) 1000

picoseconds :: Integer -> Interval
picoseconds = Interval

-- * --

toMilliseconds :: Interval -> Integer
toMilliseconds = flip div 1000000000 . coerce

toPicoseconds :: Interval -> Integer
toPicoseconds = coerce

toUTCTimeSinceEpoch :: Interval -> UTCTime
toUTCTimeSinceEpoch = TimeConversions.picosecondsSinceEpochUTCTime . coerce
