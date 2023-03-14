{-# OPTIONS_GHC -Wno-orphans #-}

module Coalmine.TransformersExtras.Reader where

import Coalmine.InternalPrelude

instance (Semigroup a, Monad m) => Semigroup (ReaderT r m a) where
  l <> r =
    (<>) <$> l <*> r

instance (Monoid a, Monad m) => Monoid (ReaderT r m a) where
  mempty = pure mempty
