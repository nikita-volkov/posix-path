module Coalmine.PartialEndo where

import Coalmine.Prelude

-- *

newtype PartialEndo a
  = PartialEndo (a -> Either (PartialEndoErr a) a)

instance Semigroup (PartialEndo a) where
  PartialEndo l <> PartialEndo r =
    PartialEndo $ \a -> case l a of
      Right a -> r a
      Left err -> Left err

instance Monoid (PartialEndo a) where
  mempty =
    PartialEndo Right

-- **

data family PartialEndoErr a
