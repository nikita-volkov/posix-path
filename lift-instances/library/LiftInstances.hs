{-# OPTIONS_GHC -Wno-orphans #-}

module LiftInstances where

import Language.Haskell.TH.Lift
import Prelude

deriving instance Lift (Fixed a)

deriveLiftMany
  [ ''Day,
    ''DiffTime,
    ''LocalTime,
    ''TimeOfDay,
    ''TimeZone,
    ''UTCTime
  ]
