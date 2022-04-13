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
    (return _base)

buildUp :: MonadPlus m => (a -> m a) -> a -> m a
buildUp k = go
  where
    go !a =
      join (mplus (go <$> k a) (pure (pure a)))

foldlMany :: MonadPlus m => (s -> a -> s) -> s -> m a -> m s
foldlMany _step _acc _get =
  _go _acc
  where
    _go !_acc =
      mplus
        (_get >>= _go . _step _acc)
        (return _acc)
