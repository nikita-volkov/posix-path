module Coalmine.Streamer where

import Coalmine.Prelude
import qualified Jsonifier

-- |
-- Specification of how to stream values of a particular type.
data Streamer a
