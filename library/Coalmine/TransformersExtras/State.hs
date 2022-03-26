module Coalmine.TransformersExtras.State where

import Coalmine.Prelude

liftModifyM :: Functor m => (s -> m s) -> StateT s m ()
liftModifyM modify =
  StateT $ fmap (fmap pure) modify
