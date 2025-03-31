{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PosixPath
  ( Path,

    -- * Accessors
    toFilePath,
    toText,
    toBasename,
    toExtensions,
    toSegments,

    -- * Partial constructors
    maybeFromText,
    maybeFromFilePath,

    -- * Constructors
    root,
    parent,
    sansParent,
    relativeTo,
  )
where

import Data.Attoparsec.Text qualified as Attoparsec
import Data.List qualified as List
import Data.Serialize qualified as Cereal
import PosixPath.Structures.Component qualified as Component
import PosixPath.Structures.Name qualified as Name
import PosixPath.Structures.Path qualified as Path
import PosixPath.Util.List qualified as List
import PosixPath.Util.Prelude hiding (null)
import Test.QuickCheck qualified as QuickCheck
import TextBuilder qualified

-- |
-- Composable automatically normalized path.
--
-- It has an instance of 'IsString',
-- so you can use string literals to define it.
-- It also has a 'toFilePath' conversion,
-- which lets you easily integrate it with any 'FilePath'-oriented libs.
--
-- === Normalization
--
-- Internally 'Path' models a normalized form,
-- making it easier to reason about and allowing to achieve the following behaviour.
--
-- The trailing slash gets ignored:
--
-- >>> "a/" :: Path
-- "./a"
--
-- Multislash gets squashed:
--
-- >>> "a//b" :: Path
-- "./a/b"
--
-- Dot-dot gets immediately applied:
--
-- >>> "a/../c" :: Path
-- "./c"
--
-- Same does the single dot:
--
-- >>> "a/./c" :: Path
-- "./a/c"
--
-- Attempts to move out of root in absolute paths are ignored:
--
-- >>> "/../a" :: Path
-- "/a"
--
-- Attempts to move out of the beginning of a relative path on the other hand are preserved:
--
-- >>> "a/../../.." :: Path
-- "../.."
--
-- Dot as the first component of the path is the same as if there's none:
--
-- >>> "./a" :: Path
-- "./a"
--
-- >>> "a" :: Path
-- "./a"
--
-- Empty path is the same as dot:
--
-- >>> "" :: Path
-- "."
--
-- === Composition
--
-- The 'Monoid' instance makes paths composable and provides the following behaviour.
--
-- Appending a relative path nests it:
--
-- >>> "/a/b" <> "c.d" :: Path
-- "/a/b/c.d"
--
-- Appending a path with dot-dot, immediately applies it:
--
-- >>> "/a/b" <> "../c" :: Path
-- "/a/c"
--
-- Appending an absolute path replaces the whole thing:
--
-- >>> "/a/b" <> "/c" :: Path
-- "/c"
data Path
  = -- | Absolute path.
    AbsNormalizedPath
      -- | Components in reverse order.
      [Name.Name]
  | RelNormalizedPath
      -- | Preceding go up commands.
      Int
      -- | Components in reverse order.
      [Name.Name]
  deriving (Eq)

instance Ord Path where
  compare = \case
    AbsNormalizedPath lNames -> \case
      AbsNormalizedPath rNames -> compare lNames rNames
      _ -> GT
    RelNormalizedPath lMovesUp lNames -> \case
      AbsNormalizedPath _ -> LT
      RelNormalizedPath rMovesUp rNames -> case compare lMovesUp rMovesUp of
        EQ -> compare lNames rNames
        res -> res

instance IsString Path where
  fromString =
    either error id
      . Attoparsec.parseOnly (attoparsecParserOf <* Attoparsec.endOfInput)
      . fromString

instance Show Path where
  show = show . toText

instance Semigroup Path where
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

instance Monoid Path where
  mempty =
    RelNormalizedPath 0 []

instance QuickCheck.Arbitrary Path where
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
        QuickCheck.vectorOf size do
          QuickCheck.suchThat QuickCheck.arbitrary (not . Name.null)
  shrink = \case
    AbsNormalizedPath names ->
      AbsNormalizedPath <$> QuickCheck.shrink names
    RelNormalizedPath movesUp names ->
      QuickCheck.shrink (movesUp, names) <&> \(movesUp, names) ->
        RelNormalizedPath movesUp names

instance Cereal.Serialize Path where
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

instance Hashable Path where
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

-- * Partial constructors

maybeFromText :: Text -> Maybe Path
maybeFromText =
  either (const Nothing) Just . Attoparsec.parseOnly (attoparsecParserOf <* Attoparsec.endOfInput)

-- | Parse file path.
maybeFromFilePath :: FilePath -> Maybe Path
maybeFromFilePath = maybeFromText . fromString

attoparsecParserOf :: Attoparsec.Parser Path
attoparsecParserOf =
  fromPath <$> Path.attoparsecParserOf

-- |
-- Normalize a path.
fromPath :: Path.Path -> Path
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

-- |
-- >>> root
-- "/"
--
-- Prepending it to a relative path will make it absolute.
root :: Path
root =
  AbsNormalizedPath []

-- * Traversers (aka Van Laarhoven lenses)

traverseNames :: (Functor f) => ([Name.Name] -> f [Name.Name]) -> Path -> f Path
traverseNames f = \case
  AbsNormalizedPath names ->
    AbsNormalizedPath <$> f names
  RelNormalizedPath movesUp names ->
    RelNormalizedPath movesUp <$> f names

traverseLastName :: (Functor f) => (Name.Name -> f Name.Name) -> Path -> f Path
traverseLastName =
  traverseNames . List.traverseHeadWithDefault Name.empty

traverseExtensions :: (Functor f) => ([Text] -> f [Text]) -> Path -> f Path
traverseExtensions =
  traverseLastName . Name.traverseExtensions

-- * Mappers

mapNames :: ([Name.Name] -> [Name.Name]) -> Path -> Path
mapNames f = \case
  AbsNormalizedPath names ->
    AbsNormalizedPath (f names)
  RelNormalizedPath movesUp names ->
    RelNormalizedPath movesUp (f names)

mapHeadName :: (Name.Name -> Name.Name) -> Path -> Path
mapHeadName f = mapNames $ \case
  head : tail -> f head : tail
  [] -> []

mapExtensions :: ([Text] -> [Text]) -> Path -> Path
mapExtensions mapper =
  runIdentity . traverseExtensions (Identity . mapper)

-- | Add file extension to the sansParent component of the path.
addExtension :: Text -> Path -> Path
addExtension ext = mapExtensions (ext :)

-- | Get the parent directory.
--
-- Is essentially the same as the following idiom:
--
-- >parent a == a <> ".."
--
-- If the path is root, the same path will be returned:
--
-- >>> parent "/"
-- "/"
--
-- If the path is relative and is either empty or points outside,
-- another level will be added.
--
-- >>> parent "."
-- ".."
--
-- >>> parent ".."
-- "../.."
--
-- For all other cases the behaviour should be self-evident:
--
-- >>> parent "/a/b"
-- "/a"
--
-- >>> parent "a/b"
-- "a"
--
-- >>> parent "a"
-- "."
--
-- >>> parent "../a"
-- ".."
parent :: Path -> Path
parent = \case
  AbsNormalizedPath names -> case names of
    _ : tail -> AbsNormalizedPath tail
    [] -> AbsNormalizedPath []
  RelNormalizedPath movesUp names -> case names of
    _ : tail -> RelNormalizedPath movesUp tail
    [] -> RelNormalizedPath (succ movesUp) []

deabsolutize :: Path -> Path
deabsolutize = \case
  AbsNormalizedPath names -> RelNormalizedPath 0 names
  relPath -> relPath

-- | Drop path to the parent directory.
--
-- >>> sansParent "/a/b"
-- "b"
--
-- >>> sansParent "a/b"
-- "b"
--
-- >>> sansParent "../a/b"
-- "b"
--
-- >>> sansParent ".."
-- ".."
--
-- >>> sansParent "."
-- "."
--
-- >>> sansParent "/"
-- "/"
sansParent :: Path -> Path
sansParent = fromNames . toNames
  where
    fromNames = \case
      head : _ -> RelNormalizedPath 0 [head]
      _ -> RelNormalizedPath 0 []

-- | Drop last extension.
sansExtension :: Path -> Path
sansExtension = mapHeadName $ Name.mapExtensions $ List.drop 1

-- * Partial mappers

-- | Given a destination path and context path, compute a path that leads from context to destination.
--
-- >>> relativeTo "a/b/c" "a/b"
-- Just ".."
--
-- >>> relativeTo "a/c" "a/b"
-- Just "../b"
--
-- >>> relativeTo "b" "a"
-- Just "../a"
--
-- >>> relativeTo "b" "."
-- Just ".."
--
-- >>> relativeTo "." "a"
-- Just "a"
--
-- >>> relativeTo "b" "/a"
-- Just "/a"
--
-- >>> relativeTo "/a/b" "/a"
-- Just ".."
--
-- It's impossible to derive a diff from an absolute path to a relative path:
--
-- >>> relativeTo "/b" "a"
-- Nothing
--
-- It's impossible to derive a diff from outside:
--
-- >>> relativeTo ".." "a"
-- Nothing
--
-- You can view this as a sort of a subtraction operation.
relativeTo ::
  Path ->
  Path ->
  Maybe Path
relativeTo = \case
  RelNormalizedPath _targetMovesUp _targetComponents -> \case
    RelNormalizedPath _sourceMovesUp _sourceComponents ->
      error "TODO"
    AbsNormalizedPath _sourceComponents ->
      error "TODO"
  AbsNormalizedPath _targetComponents ->
    error "TODO"

-- |
-- >>> without "b/c" "a/b/c"
-- Just "./a"
--
-- >>> without "b/c" "/a/b/c"
-- Just "./a"
--
-- >>> without "b/c" "a/b"
-- Nothing
--
-- >>> without "/b/c" "/a/b/c"
-- Nothing
--
-- >>> without "a" "a"
-- .
--
-- >>> without "/b/c" "/b/c"
-- .
without :: Path -> Path -> Maybe Path
without =
  error "TODO"

-- * Accessors

toTextBuilder :: Path -> TextBuilder.TextBuilder
toTextBuilder =
  Path.toTextBuilder . toPath

-- | Compile to standard file path string.
toFilePath :: Path -> FilePath
toFilePath = toList . toText

-- | Compile to text.
toText :: Path -> Text
toText = TextBuilder.run . toTextBuilder

toPath :: Path -> Path.Path
toPath = \case
  AbsNormalizedPath names ->
    Path.Path True (fmap Component.NameComponent names)
  RelNormalizedPath movesUp names ->
    Path.Path False
      $ if movesUp == 0
        then
          fmap Component.NameComponent names
            <> pure Component.DotComponent
        else
          fmap Component.NameComponent names
            <> replicate movesUp Component.DotDotComponent

-- |
-- Decompose into individual components.
toSegments :: Path -> [Path]
toSegments = \case
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

toNames :: Path -> [Name.Name]
toNames = \case
  AbsNormalizedPath names -> names
  RelNormalizedPath _ names -> names

-- | File name sans extensions.
toBasename :: Path -> Text
toBasename path =
  case toNames path of
    head : _ -> Name.toBase head
    _ -> mempty

toExtensions :: Path -> [Text]
toExtensions =
  reverse . Name.toExtensions . List.headOr Name.empty . toNames
