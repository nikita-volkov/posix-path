{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}

module LiftInstances where

import Language.Haskell.TH.Lift
import Prelude

#if __GLASGOW_HASKELL__ < 908
deriving instance Lift (Fixed a)
#endif

deriveLiftMany
  [ ''Day,
    ''DiffTime,
    ''LocalTime,
    ''TimeOfDay,
    ''TimeZone,
    ''UTCTime
  ]
