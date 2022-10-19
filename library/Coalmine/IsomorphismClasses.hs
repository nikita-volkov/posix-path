-- |
-- Compared to Lingua does not force a single association
-- by letting the user disambiguate in ambiguous cases via TypeApplications.
module Coalmine.IsomorphismClasses where

import Coalmine.InternalPrelude
import qualified Data.Map
import qualified Data.Map.Strict
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder

class Strictness lazy strict where
  toStrict :: lazy -> strict
  toLazy :: strict -> lazy

class Freezing melted frozen where
  freeze :: melted -> frozen
  melt :: frozen -> melted

instance Freezing Data.Text.Lazy.Builder.Builder Data.Text.Lazy.Text where
  freeze = Data.Text.Lazy.Builder.toLazyText
  melt = Data.Text.Lazy.Builder.fromLazyText
