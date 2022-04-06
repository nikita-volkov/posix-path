module Coalmine.BaseExtras.MonadPlus where

import Coalmine.InternalPrelude

-- |
-- Compose a monad, which attempts to extend a value, based on the following input.
-- It does that recursively until the suffix alternative fails.
recurseExtending :: (MonadPlus m) => m a -> (a -> m a) -> m a
recurseExtending base suffix = do
  _base <- base
  mplus
    (recurseExtending (suffix _base) suffix)
    (pure _base)
