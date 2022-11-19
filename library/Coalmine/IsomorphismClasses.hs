-- |
-- Collection of standard isomorphisms under the interface of classes.
--
-- Compared to Lingua does not force a single association
-- by letting the user disambiguate in ambiguous cases via TypeApplications.
module Coalmine.IsomorphismClasses where

import Coalmine.InternalPrelude
import Data.Map qualified
import Data.Map.Strict qualified
import Data.Text qualified
import Data.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified
import Text.Builder qualified
import TextBuilderDev qualified

class Strictness lazy strict where
  toStrict :: lazy -> strict
  toLazy :: strict -> lazy

class Freezing melted frozen where
  freeze :: melted -> frozen
  melt :: frozen -> melted

instance Freezing Data.Text.Lazy.Builder.Builder Data.Text.Lazy.Text where
  freeze = Data.Text.Lazy.Builder.toLazyText
  melt = Data.Text.Lazy.Builder.fromLazyText

instance Freezing TextBuilderDev.TextBuilder Data.Text.Text where
  freeze = TextBuilderDev.buildText
  melt = TextBuilderDev.text

instance Freezing Text.Builder.Builder Data.Text.Text where
  freeze = Text.Builder.run
  melt = Text.Builder.text
