-- | Report about an error.
module Coalmine.ErrReport where

import Coalmine.UserErr qualified as UserErr

-- |
-- Layers of user error contexts.
-- With the higher precision the deeper you traverse.
-- The last element is the root cause.
--
-- DEPRECATED:
-- Seems to pointlessly complicate the error construction,
-- requiring to specify reasoning on all levels.
-- Instead it should be enough to just provide the coordinates of what happened.
newtype ErrReport = ErrReport [UserErr.UserErr]
