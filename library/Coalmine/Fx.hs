module Coalmine.Fx where

import Prelude
import Fx


runFxFailing :: Show err => Fx () err a -> IO a
runFxFailing = runFx . handleErr (fail . show)
