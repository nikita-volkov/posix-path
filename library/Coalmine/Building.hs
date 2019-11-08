module Coalmine.Building where

import Prelude
import qualified Text.Builder as TextBuilder


class Monoid builder => Building builder where
  type BuilderTarget builder
  toBuilder :: BuilderTarget builder -> builder
  fromBuilder :: builder -> BuilderTarget builder

instance Building TextBuilder.Builder where
  type BuilderTarget TextBuilder.Builder = Text
  toBuilder = TextBuilder.text
  fromBuilder = TextBuilder.run
