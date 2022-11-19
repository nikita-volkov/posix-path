module Coalmine.CasedText where

import Coalmine.InternalPrelude
import Coalmine.MultilineTextBuilder qualified as MultilineTextBuilder
import Coalmine.Name qualified as Name
import Coalmine.Name.Attoparsec qualified as Attoparsec
import Coalmine.Name.Constants qualified as Constants
import Coalmine.Name.Gens qualified as Gens
import Coalmine.Name.Megaparsec qualified as Megaparsec
import Coalmine.Parsing
import Coalmine.Printing
import Coalmine.QuickCheckExtras.Gens qualified as QuickCheckExtrasGens
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Serialize qualified as Cereal
import Data.Text qualified as Text
import Test.QuickCheck qualified as QuickCheck
import Text.Megaparsec qualified as Megaparsec
import TextBuilderDev qualified as TextBuilder

data CasedText = CasedText
  { titlingPolicy :: !TitlingPolicy,
    stitchingPolicy :: !StitchingPolicy,
    parts :: !(BVec Part)
  }
  deriving (Eq, Ord)

data TitlingPolicy
  = AllTitlingPolicy
  | FirstTitlingPolicy
  | NoneTitlingPolicy
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance Hashable TitlingPolicy

instance QuickCheck.Arbitrary TitlingPolicy where
  arbitrary = QuickCheckExtrasGens.enumBounded

data StitchingPolicy
  = CamelStitchingPolicy
  | SnakeStitchingPolicy
  | SpinalStitchingPolicy
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance Hashable StitchingPolicy

instance QuickCheck.Arbitrary StitchingPolicy where
  arbitrary = QuickCheckExtrasGens.enumBounded

instance CompactPrinting CasedText where
  toCompactBuilder casedText =
    case casedText.stitchingPolicy of
      CamelStitchingPolicy ->
        error "TODO"
      SnakeStitchingPolicy ->
        error "TODO"
      SpinalStitchingPolicy ->
        error "TODO"

data Part = Part
  { head :: !Char,
    tail :: !Text
  }
  deriving (Eq, Ord)
