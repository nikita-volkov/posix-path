module Coalmine.TimeExtras.IO where

import Coalmine.InternalPrelude
import Coalmine.TimeExtras.Conversions qualified as Conversions
import Data.Time.Clock.System

getMicrosSinceEpoch :: IO Int64
getMicrosSinceEpoch =
  Conversions.systemTimeInt64MicrosSinceEpoch <$> getSystemTime
