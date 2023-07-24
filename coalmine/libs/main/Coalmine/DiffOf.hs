module Coalmine.DiffOf where

import Coalmine.InternalPrelude

-- | Relation between a value and an optimal representation of change in it.
class DiffOf struct diff | struct -> diff where
  applyDiff :: diff -> struct -> struct
  deriveDiff :: struct -> struct -> diff

instance DiffOf UTCTime NominalDiffTime where
  applyDiff = addUTCTime
  deriveDiff = diffUTCTime

instance DiffOf Int Int where
  applyDiff = (+)
  deriveDiff l r = r - l
