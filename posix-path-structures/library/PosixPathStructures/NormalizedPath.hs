module PosixPathStructures.NormalizedPath
  ( NormalizedPath (..),

    -- * Constructors
    root,
    fromPath,
    addExtension,

    -- * Accessors
    toFilePath,
    toText,
    toPath,
    decompose,
    basename,
    extensions,
    parent,
    deabsolutize,
    dropParent,
    dropExtension,
  )
where

import Coalmine.BaseExtras.List qualified as List
import Coalmine.Prelude hiding (null)
import Coalmine.SyntaxModelling qualified as Syntax
import Data.List qualified as List
import Data.Serialize qualified as Cereal
import PosixPathStructures.Component qualified as Component
import PosixPathStructures.Name qualified as Name
import PosixPathStructures.Path qualified as Path
import Test.QuickCheck qualified as QuickCheck

-- |
-- Composable automatically normalized path.
data NormalizedPath
  = -- | Absolute path.
    AbsNormalizedPath
      ![Name.Name]
      -- ^ Components in reverse order.
  | RelNormalizedPath
      !Int
      -- ^ Preceding go up commands.
      ![Name.Name]
      -- ^ Components in reverse order.
  deriving (Eq, Show)

instance Ord NormalizedPath where
  compare = \case
    AbsNormalizedPath lNames -> \case
      AbsNormalizedPath rNames -> compare lNames rNames
      _ -> GT
    RelNormalizedPath lMovesUp lNames -> \case
      AbsNormalizedPath _ -> LT
      RelNormalizedPath rMovesUp rNames -> case compare lMovesUp rMovesUp of
        EQ -> compare lNames rNames
        res -> res

instance IsString NormalizedPath where
  fromString = Syntax.fromStringUnsafe

instance Semigroup NormalizedPath where
  lPath <> rPath = case rPath of
    AbsNormalizedPath _ -> rPath
    RelNormalizedPath rMovesUp rNames ->
      case lPath of
        RelNormalizedPath lMovesUp lNames ->
          case List.dropPedantically rMovesUp lNames of
            Left rMovesUp -> RelNormalizedPath (rMovesUp + lMovesUp) rNames
            Right lNames -> RelNormalizedPath lMovesUp (rNames <> lNames)
        AbsNormalizedPath lNames ->
          AbsNormalizedPath (rNames <> drop rMovesUp lNames)

instance Monoid NormalizedPath where
  mempty =
    RelNormalizedPath 0 []

instance QuickCheck.Arbitrary NormalizedPath where
  arbitrary =
    QuickCheck.oneof [abs, rel]
    where
      abs =
        AbsNormalizedPath <$> names
      rel =
        RelNormalizedPath <$> movesUp <*> names
        where
          movesUp =
            QuickCheck.chooseInt (0, 3)
      names = do
        size <- QuickCheck.chooseInt (0, 20)
        QuickCheck.vectorOf size QuickCheck.arbitrary
  shrink = \case
    AbsNormalizedPath names ->
      AbsNormalizedPath <$> QuickCheck.shrink names
    RelNormalizedPath movesUp names ->
      QuickCheck.shrink (movesUp, names) <&> \(movesUp, names) ->
        RelNormalizedPath movesUp names

instance Cereal.Serialize NormalizedPath where
  put = \case
    AbsNormalizedPath names -> do
      Cereal.put @Word8 0
      Cereal.put names
    RelNormalizedPath movesUp names -> do
      Cereal.put @Word8 1
      Cereal.put movesUp
      Cereal.put names
  get = do
    tag <- Cereal.get @Word8
    case tag of
      0 -> do
        names <- Cereal.get
        return $ AbsNormalizedPath names
      1 -> do
        movesUp <- Cereal.get
        names <- Cereal.get
        return $ RelNormalizedPath movesUp names
      _ -> fail $ "Invalid tag: " <> show tag

instance Syntax.Syntax NormalizedPath where
  attoparsecParser =
    fromPath <$> Syntax.attoparsecParser
  toTextBuilder =
    Syntax.toTextBuilder . toPath

instance Hashable NormalizedPath where
  hashWithSalt salt = \case
    AbsNormalizedPath names ->
      salt
        & extendHash @Int 0
        & extendHash names
    RelNormalizedPath movesUp names ->
      salt
        & extendHash @Int 1
        & extendHash movesUp
        & extendHash names
    where
      extendHash :: (Hashable a) => a -> Int -> Int
      extendHash = flip hashWithSalt

-- | Compile to standard file path string.
toFilePath :: NormalizedPath -> FilePath
toFilePath = to @String . Syntax.toTextBuilder

-- | Compile to text.
toText :: NormalizedPath -> Text
toText = to . Syntax.toTextBuilder

-- |
-- Normalize a path.
fromPath :: Path.Path -> NormalizedPath
fromPath (Path.Path root components) =
  foldr step finish components 0 []
  where
    step component next !collectedMovesUp !collectedNames =
      case component of
        Component.NameComponent name ->
          if collectedMovesUp > 0
            then next (pred collectedMovesUp) collectedNames
            else
              if Name.null name
                then next collectedMovesUp collectedNames
                else next collectedMovesUp (name : collectedNames)
        Component.DotComponent ->
          next collectedMovesUp collectedNames
        Component.DotDotComponent ->
          next (succ collectedMovesUp) collectedNames
    finish collectedMovesUp collectedNames =
      if root
        then AbsNormalizedPath (reverse collectedNames)
        else RelNormalizedPath collectedMovesUp (reverse collectedNames)

toPath :: NormalizedPath -> Path.Path
toPath = \case
  AbsNormalizedPath names ->
    Path.Path True (fmap Component.NameComponent names)
  RelNormalizedPath movesUp names ->
    Path.Path False $
      if movesUp == 0
        then
          fmap Component.NameComponent names
            <> pure Component.DotComponent
        else
          fmap Component.NameComponent names
            <> replicate movesUp Component.DotDotComponent

-- |
-- Prepending it to a relative path will make it absolute.
root :: NormalizedPath
root =
  AbsNormalizedPath []

-- |
-- Explode into individual components.
decompose :: NormalizedPath -> [NormalizedPath]
decompose = \case
  AbsNormalizedPath names ->
    case reverse names of
      head : tail ->
        AbsNormalizedPath [head]
          : fmap (RelNormalizedPath 0 . pure) tail
      _ ->
        [AbsNormalizedPath []]
  RelNormalizedPath movesUp names ->
    replicate movesUp (RelNormalizedPath 1 [])
      <> fmap (RelNormalizedPath 0 . pure) (reverse names)

-- * Traversers (aka Van Laarhoven lenses)

traverseNames :: (Functor f) => ([Name.Name] -> f [Name.Name]) -> NormalizedPath -> f NormalizedPath
traverseNames f = \case
  AbsNormalizedPath names ->
    AbsNormalizedPath <$> f names
  RelNormalizedPath movesUp names ->
    RelNormalizedPath movesUp <$> f names

traverseLastName :: (Functor f) => (Name.Name -> f Name.Name) -> NormalizedPath -> f NormalizedPath
traverseLastName =
  traverseNames . List.traverseHeadWithDefault Name.empty

traverseExtensions :: (Functor f) => ([Text] -> f [Text]) -> NormalizedPath -> f NormalizedPath
traverseExtensions =
  traverseLastName . Name.traverseExtensions

-- * Mappers

mapNames :: ([Name.Name] -> [Name.Name]) -> NormalizedPath -> NormalizedPath
mapNames f = \case
  AbsNormalizedPath names ->
    AbsNormalizedPath (f names)
  RelNormalizedPath movesUp names ->
    RelNormalizedPath movesUp (f names)

mapHeadName :: (Name.Name -> Name.Name) -> NormalizedPath -> NormalizedPath
mapHeadName f = mapNames $ \case
  head : tail -> f head : tail
  [] -> []

mapExtensions :: ([Text] -> [Text]) -> NormalizedPath -> NormalizedPath
mapExtensions mapper =
  runIdentity . traverseExtensions (Identity . mapper)

-- | Add file extension to the dropParent component of the path.
addExtension :: Text -> NormalizedPath -> NormalizedPath
addExtension ext = mapExtensions (ext :)

-- * Accessors

names :: NormalizedPath -> [Name.Name]
names = \case
  AbsNormalizedPath names -> names
  RelNormalizedPath _ names -> names

-- | File name sans extensions.
basename :: NormalizedPath -> Text
basename path =
  case names path of
    head : _ -> head.base
    _ -> mempty

extensions :: NormalizedPath -> [Text]
extensions =
  reverse . (.extensions) . List.headOr Name.empty . names

parent :: NormalizedPath -> NormalizedPath
parent = \case
  AbsNormalizedPath names -> case names of
    _ : tail -> AbsNormalizedPath tail
    [] -> AbsNormalizedPath []
  RelNormalizedPath movesUp names -> case names of
    _ : tail -> RelNormalizedPath movesUp tail
    [] -> RelNormalizedPath (succ movesUp) []

deabsolutize :: NormalizedPath -> NormalizedPath
deabsolutize = \case
  AbsNormalizedPath names -> RelNormalizedPath 0 names
  relPath -> relPath

-- | Drop path to the parent directory.
dropParent :: NormalizedPath -> NormalizedPath
dropParent = fromNames . names
  where
    fromNames = \case
      head : _ -> RelNormalizedPath 0 [head]
      _ -> RelNormalizedPath 0 []

-- | Drop last extension.
dropExtension :: NormalizedPath -> NormalizedPath
dropExtension = mapHeadName $ Name.mapExtensions $ List.drop 1
