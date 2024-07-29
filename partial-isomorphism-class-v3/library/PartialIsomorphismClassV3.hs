module PartialIsomorphismClassV3 where

import Coalmine.Prelude
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Encoding

-- TODO: Rename to refinement.
-- Implying that the type is a sort of an extension to some base type,
-- hence meaining that it may only have one base type and the base type may have multiple refinements.
class Refinement a where
  type Polluted a
  type RefinementError a
  pollute :: a -> Polluted a
  refine :: Polluted a -> Either (RefinementError a) a

instance Refinement Text where
  type Polluted Text = ByteString
  type RefinementError Text = Text.Encoding.UnicodeException
  pollute = Text.Encoding.encodeUtf8
  refine = Text.Encoding.decodeUtf8'
