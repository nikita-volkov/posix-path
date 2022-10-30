module Coalmine.CasedText where

import Coalmine.InternalPrelude
import qualified Coalmine.MultilineTextBuilder as MultilineTextBuilder
import qualified Coalmine.Name as Name
import qualified Coalmine.Name.Attoparsec as Attoparsec
import qualified Coalmine.Name.Constants as Constants
import qualified Coalmine.Name.Gens as Gens
import qualified Coalmine.Name.Megaparsec as Megaparsec
import Coalmine.Parsing
import Coalmine.Printing
import qualified Coalmine.QuickCheckExtras.Gens as QuickCheckExtrasGens
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Serialize as Cereal
import qualified Data.Text as Text
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Megaparsec as Megaparsec
import qualified TextBuilderDev as TextBuilder

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
