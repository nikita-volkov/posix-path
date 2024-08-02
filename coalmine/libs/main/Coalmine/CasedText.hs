-- TODO: Consider renaming to Uncased.
module Coalmine.CasedText where

import Coalmine.InternalPrelude
import Coalmine.Printing
import Coalmine.QuickCheckExtras.Gens qualified as QuickCheckExtrasGens
import Test.QuickCheck qualified as QuickCheck

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
