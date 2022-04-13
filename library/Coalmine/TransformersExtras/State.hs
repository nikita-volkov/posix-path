module Coalmine.TransformersExtras.State where

import Coalmine.InternalPrelude

liftModifyM :: Functor m => (s -> m s) -> StateT s m ()
liftModifyM modify =
  StateT $ fmap (fmap pure) modify

unlessMissing :: (MonadError e m) => e -> StateT s m a -> StateT (Maybe s) m a
unlessMissing missingErr m =
  StateT $ \case
    Just x -> do
      (r, x) <- runStateT m x
      return (r, Just x)
    Nothing ->
      throwError missingErr
