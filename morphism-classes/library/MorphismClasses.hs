module MorphismClasses where

import Coalmine.Prelude
import Data.Text.Lazy qualified as Text.Lazy
import TextBuilderDev qualified

class Builds builder where
  type Built builder
  toBuilder :: Built builder -> builder
  fromBuilder :: builder -> Built builder

instance Builds TextBuilderDev.TextBuilder where
  type Built TextBuilderDev.TextBuilder = Text
  toBuilder = TextBuilderDev.text
  fromBuilder = TextBuilderDev.buildText

-- * Lazy

class Lazy lazy where
  type Strict lazy
  toLazy :: Strict lazy -> lazy
  fromLazy :: lazy -> Strict lazy

instance Lazy Text.Lazy.Text where
  type Strict Text.Lazy.Text = Text
  toLazy = Text.Lazy.fromStrict
  fromLazy = Text.Lazy.toStrict
