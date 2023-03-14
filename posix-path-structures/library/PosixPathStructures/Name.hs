module PosixPathStructures.Name
  ( Name (..),
    null,
    empty,
    traverseExtensions,
    mapExtensions,
  )
where

import Algorithms.NaturalSort qualified as NaturalSort
import Coalmine.BaseExtras.List qualified as List
import Coalmine.BaseExtras.MonadPlus
import Coalmine.EvenSimplerPaths.AttoparsecHelpers qualified as AttoparsecHelpers
import Coalmine.EvenSimplerPaths.IsomorphismClassHelpers qualified as IsomorphismClassHelpers
import Coalmine.EvenSimplerPaths.QuickCheckGens qualified as QuickCheckGens
import Coalmine.Prelude hiding (empty, null, Name)
import Coalmine.SyntaxModelling qualified as Syntax
import Data.Attoparsec.Text qualified as Attoparsec
import Data.List qualified as List
import Data.Serialize qualified as Cereal
import Data.Text qualified as Text
import Test.QuickCheck qualified as QuickCheck
import TextBuilderDev qualified as TextBuilderDev

-- |
-- Structured base of a single component of a path.
data Name = Name
  { -- | Name.
    base :: !Text,
    -- | Extensions in reverse order.
    extensions :: ![Text]
  }
  deriving (Eq, Show)

instance QuickCheck.Arbitrary Name where
  arbitrary = do
    base <- QuickCheckGens.fileName
    fileExtensions <- QuickCheckGens.fileExtensions
    return $ Name base fileExtensions
  shrink (Name base extensions) =
    QuickCheck.shrink (base, extensions) <&> \(base, extensions) ->
      Name base (List.filter (not . Text.null) extensions)

instance Cereal.Serialize Name where
  put (Name base extensions) = do
    Cereal.put base
    Cereal.put extensions
  get = do
    base <- Cereal.get
    extensions <- Cereal.get
    return $ Name base extensions

instance Ord Name where
  compare l r =
    if la == ra
      then
        if lb < rb
          then LT
          else
            if lb == rb
              then EQ
              else GT
      else
        if la < ra
          then LT
          else GT
    where
      la = baseSortKey l
      lb = extensionsSortKey l
      ra = baseSortKey r
      rb = extensionsSortKey r

instance Syntax.Syntax Name where
  attoparsecParser = do
    base <- AttoparsecHelpers.fileName
    extensions <- reverseMany AttoparsecHelpers.extension
    return $ Name base extensions
  toTextBuilder (Name base extensions) =
    foldr
      (\extension next -> next <> "." <> to extension)
      (to base)
      extensions

instance Hashable Name where
  hashWithSalt salt Name {..} =
    salt
      & extendHash base
      & extendHash extensions
    where
      extendHash = flip hashWithSalt

baseSortKey :: Name -> NaturalSort.SortKey
baseSortKey = NaturalSort.sortKey . (.base)

extensionsSortKey :: Name -> [NaturalSort.SortKey]
extensionsSortKey = reverse . fmap NaturalSort.sortKey . (.extensions)

null :: Name -> Bool
null (Name name extensions) =
  Text.null name && List.null extensions

empty :: Name
empty =
  Name mempty mempty

traverseExtensions :: (Functor f) => ([Text] -> f [Text]) -> Name -> f Name
traverseExtensions f (Name base extensions) =
  Name base <$> f extensions

mapExtensions :: ([Text] -> [Text]) -> Name -> Name
mapExtensions f (Name base extensions) =
  Name base (f extensions)
