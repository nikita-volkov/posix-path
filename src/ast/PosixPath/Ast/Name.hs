module PosixPath.Ast.Name
  ( Name,

    -- * Constructors
    empty,
    mapExtensions,

    -- * Functors
    traverseExtensions,
    attoparsecParserOf,

    -- * Destructors
    null,
    toTextBuilder,
    toText,
    toBase,
    toExtensions,
  )
where

import Algorithms.NaturalSort qualified as NaturalSort
import Data.Attoparsec.Text qualified as Attoparsec
import Data.List qualified as List
import Data.Serialize qualified as Cereal
import Data.Serialize.Text ()
import Data.Text qualified as Text
import PosixPath.Ast.Name.NameSegment qualified as NameSegment
import PosixPath.Util.MonadPlus
import PosixPath.Util.Prelude hiding (empty, null)
import Test.QuickCheck qualified as QuickCheck
import TextBuilder qualified

-- |
-- Structured base of a single component of a path.
data Name = Name
  { -- | Name.
    base :: Text,
    -- | Extensions in reverse order.
    extensions :: [Text]
  }
  deriving (Eq, Show)

instance QuickCheck.Arbitrary Name where
  arbitrary = do
    base <-
      QuickCheck.oneof
        [ NameSegment.toText <$> arbitrary,
          pure ""
        ]
    extensions <- fmap NameSegment.toText <$> arbitrary
    pure (Name base extensions)
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
      la = toBaseSortKey l
      lb = toExtensionsSortKey l
      ra = toBaseSortKey r
      rb = toExtensionsSortKey r

instance Hashable Name where
  hashWithSalt salt Name {..} =
    salt
      & extendHash base
      & extendHash extensions
    where
      extendHash = flip hashWithSalt

-- * Constructors

empty :: Name
empty =
  Name mempty mempty

mapExtensions :: ([Text] -> [Text]) -> Name -> Name
mapExtensions f = runIdentity . traverseExtensions (Identity . f)

maybeFromText :: Text -> Maybe Name
maybeFromText text =
  Attoparsec.parseOnly (attoparsecParserOf <* Attoparsec.endOfInput) text
    & either (const Nothing) Just

-- * Functors

traverseExtensions :: (Functor f) => ([Text] -> f [Text]) -> Name -> f Name
traverseExtensions f (Name base extensions) =
  f extensions
    & fmap
      ( \list ->
          list
            & concatMap
              ( \extension ->
                  case maybeFromText extension of
                    Just (Name base' extensions') ->
                      (extensions' <> [base'])
                        & filter (not . Text.null)
                    Nothing -> []
              )
            & Name base
      )

attoparsecParserOf :: Attoparsec.Parser Name
attoparsecParserOf = do
  base <- NameSegment.attoparsecParserOf <|> pure ""
  extensions <- reverseMany (Attoparsec.char '.' *> NameSegment.attoparsecParserOf)
  return (Name base extensions)

-- * Destructors

toBase :: Name -> Text
toBase (Name base _) =
  base

toExtensions :: Name -> [Text]
toExtensions (Name _ extensions) =
  extensions

toTextBuilder :: Name -> TextBuilder.TextBuilder
toTextBuilder (Name base extensions) =
  foldr
    (\extension next -> next <> "." <> TextBuilder.text extension)
    (TextBuilder.text base)
    extensions

toText :: Name -> Text
toText = TextBuilder.run . toTextBuilder

toBaseSortKey :: Name -> NaturalSort.SortKey
toBaseSortKey = NaturalSort.sortKey . toBase

toExtensionsSortKey :: Name -> [NaturalSort.SortKey]
toExtensionsSortKey = reverse . fmap NaturalSort.sortKey . toExtensions

null :: Name -> Bool
null (Name name extensions) =
  Text.null name && List.null extensions
