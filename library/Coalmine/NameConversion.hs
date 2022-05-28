module Coalmine.NameConversion where

import Coalmine.InternalPrelude
import qualified Coalmine.MultilineTextBuilder as MultilineTextBuilder
import Coalmine.Name

-- * --

-- | Name rendering letter case.
data FromNameCase
  = SpinalFromNameCase
  | SnakeFromNameCase
  | UpperCamelFromNameCase
  | LowerCamelFromNameCase

instance IsLabel "spinal" FromNameCase where
  fromLabel = SpinalFromNameCase

instance IsLabel "snake" FromNameCase where
  fromLabel = SnakeFromNameCase

instance IsLabel "upperCamel" FromNameCase where
  fromLabel = UpperCamelFromNameCase

instance IsLabel "lowerCamel" FromNameCase where
  fromLabel = LowerCamelFromNameCase

-- * --

class FromName a where
  fromNameIn :: FromNameCase -> Name -> a

instance FromName Text where
  fromNameIn = \case
    SpinalFromNameCase -> toSpinalCaseText
    SnakeFromNameCase -> toSnakeCaseText
    UpperCamelFromNameCase -> toUpperCamelCaseText
    LowerCamelFromNameCase -> toLowerCamelCaseText

instance FromName TextBuilder where
  fromNameIn = \case
    SpinalFromNameCase -> toSpinalCaseTextBuilder
    SnakeFromNameCase -> toSnakeCaseTextBuilder
    UpperCamelFromNameCase -> toUpperCamelCaseTextBuilder
    LowerCamelFromNameCase -> toLowerCamelCaseTextBuilder

instance FromName MultilineTextBuilder.Builder where
  fromNameIn casing = from @TextBuilder . fromNameIn casing
