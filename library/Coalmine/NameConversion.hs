module Coalmine.NameConversion where

import Coalmine.InternalPrelude
import qualified Coalmine.MultilineTextBuilder as MultilineTextBuilder
import Coalmine.Name

-- | Name rendering letter case.
data NameCase
  = LowerSpinalNameCase
  | LowerSnakeNameCase
  | UpperCamelNameCase
  | LowerCamelNameCase

class RenderNameTo a where
  renderNameTo :: NameCase -> Name -> a

instance RenderNameTo Text where
  renderNameTo = \case
    LowerSpinalNameCase -> toSpinalCaseText

instance RenderNameTo TextBuilder where
  renderNameTo = \case
    LowerSpinalNameCase -> toSpinalCaseTextBuilder

instance RenderNameTo MultilineTextBuilder.Builder where
  renderNameTo = \case
    LowerSpinalNameCase -> to . toSpinalCaseTextBuilder
