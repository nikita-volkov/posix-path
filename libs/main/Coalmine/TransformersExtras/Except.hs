module Coalmine.TransformersExtras.Except where

import Coalmine.InternalPrelude

instance (Semigroup a, Monad m) => Semigroup (ExceptT r m a) where
  l <> r =
    (<>) <$> l <*> r

instance (Monoid a, Monad m) => Monoid (ExceptT r m a) where
  mempty = pure mempty

instance (IsString a, Monad m) => IsString (ExceptT r m a) where
  fromString = return . fromString
