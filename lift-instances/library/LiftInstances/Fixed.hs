{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

module LiftInstances.Fixed where

import Data.Fixed
import Language.Haskell.TH.Lift

#if __GLASGOW_HASKELL__ < 908
deriving instance Lift (Fixed a)
#endif
