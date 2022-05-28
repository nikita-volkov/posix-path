module Coalmine.NameConversion where

import Coalmine.InternalPrelude
import qualified Coalmine.MultilineTextBuilder as MultilineTextBuilder
import Coalmine.Name

-- | Name rendering letter case.
data FromNameCase
  = SpinalFromNameCase
  | SnakeFromNameCase
  | UpperCamelFromNameCase
  | LowerCamelFromNameCase

class FromName a where
  fromName :: FromNameCase -> Name -> a

instance FromName Text where
  fromName = \case
    SpinalFromNameCase -> toSpinalCaseText
    SnakeFromNameCase -> toSnakeCaseText
    UpperCamelFromNameCase -> toUpperCamelCaseText
    LowerCamelFromNameCase -> toLowerCamelCaseText

instance FromName TextBuilder where
  fromName = \case
    SpinalFromNameCase -> toSpinalCaseTextBuilder
    SnakeFromNameCase -> toSnakeCaseTextBuilder
    UpperCamelFromNameCase -> toUpperCamelCaseTextBuilder
    LowerCamelFromNameCase -> toLowerCamelCaseTextBuilder

instance FromName MultilineTextBuilder.Builder where
  fromName casing = from @TextBuilder . fromName casing
