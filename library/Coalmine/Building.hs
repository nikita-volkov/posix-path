module Coalmine.Building where

import TextBuilderDev (TextBuilder)
import qualified TextBuilderDev as TextBuilder
import Prelude

class Monoid builder => Building builder where
  type BuilderTarget builder
  toBuilder :: BuilderTarget builder -> builder
  fromBuilder :: builder -> BuilderTarget builder

instance Building TextBuilder where
  type BuilderTarget TextBuilder = Text
  toBuilder = TextBuilder.text
  fromBuilder = TextBuilder.buildText
