module PosixPath
  ( Path,

    -- * Accessors
    toFilePath,
    toText,
    toBasename,
    toExtensions,
    toSegments,
    isAbsolute,

    -- * Partial constructors
    maybeFromText,
    maybeFromFilePath,

    -- * Parsers
    attoparsecParserOf,

    -- * Constructors
    root,
    dropParent,
    dropLastSegment,
    addExtension,
    dropExtension,
    dropExtensions,
    deabsolutize,

    -- * Idioms

    -- | Getting the parent directory
    --
    -- >>> "a/b" <> ".." :: Path
    -- "./a"

    -- | Isolating to just the file name
    --
    -- >>> (dropParent . dropExtensions) "/a/b.c.d" :: Path
    -- "./b"
  )
where

import Data.Attoparsec.Text qualified as Attoparsec
import Data.List qualified as List
import Data.Serialize qualified as Cereal
import PosixPath.Ast.Component qualified as Ast.Component
import PosixPath.Ast.Name qualified as Ast.Name
import PosixPath.Ast.Path qualified as Ast.Path
import PosixPath.Util.List qualified as List
import PosixPath.Util.Prelude hiding (null)
import Test.QuickCheck qualified as QuickCheck
import TextBuilder qualified

-- |
-- Composable normalized path with a powerful algebra and consistent behaviour.
--
-- - It has 'toFilePath' and 'maybeFromFilePath' conversions,
-- which let you easily integrate it with any 'FilePath'-oriented libs.
-- - It has an instance of 'IsString',
-- so you can use string literals to define it.
-- - It provides a 'Monoid' instance replacing the `(</>)` operator of other libs with a standard lawful abstraction.
-- - It is automatically normalized, ensuring that various representations of the same path are equal, produce equal hashes and get ordered the same.
-- - It implements [natural sorting](https://en.wikipedia.org/wiki/Natural_sort_order).
-- - It integrates with 'Text' using 'toText' and 'maybeFromText'.
-- - It integrates with \"attoparsec\" via 'attoparsecParserOf'.
--
-- === Normalization
--
-- Internally 'Path' models a normalized form,
-- removing many ambiguities and allowing to achieve the following behaviour.
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
-- Empty path is the same as dot:
--
-- >>> mempty :: Path
-- "."
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
--
-- === Consistent equality, order and hashing
--
-- Thanks to 'Path' being in normalized form, the equality is consistent:
--
-- >>> ("./a/b" :: Path) == "a/b"
-- True
--
-- >>> ("./a/b" :: Path) == "a/b/"
-- True
--
-- >>> ("./a/b" :: Path) == "a//b/"
-- True
--
-- >>> ("./a/b" :: Path) == "a/../a/b/"
-- True
--
-- None of the above would hold for 'FilePath'.
--
-- Same logic applies to ordering and hashing.
--
-- === Natural sorting
--
-- The path is sorted [naturally](https://en.wikipedia.org/wiki/Natural_sort_order), meaning that the segments are compared as numbers if they are all digits.
-- This is useful for sorting paths with version numbers:
--
-- >>> import Data.List (sort)
-- >>> sort ["a/1", "a/11", "a/20", "a/2", "a/10"] :: [Path]
-- ["./a/1","./a/2","./a/10","./a/11","./a/20"]
--
-- where with 'FilePath' you would get:
--
-- >>> sort ["a/1", "a/11", "a/20", "a/2", "a/10"] :: [FilePath]
-- ["a/1","a/10","a/11","a/2","a/20"]
data Path
  = -- | Absolute path.
    AbsNormalizedPath
      -- | Ast.Components in reverse order.
      [Ast.Name.Name]
  | RelNormalizedPath
      -- | Preceding go up commands.
      Int
      -- | Ast.Components in reverse order.
      [Ast.Name.Name]
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

-- | Renders as a string literal.
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
          QuickCheck.suchThat QuickCheck.arbitrary (not . Ast.Name.null)
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

-- | Attempt to parse a path from text.
maybeFromText :: Text -> Maybe Path
maybeFromText =
  either (const Nothing) Just . Attoparsec.parseOnly (attoparsecParserOf <* Attoparsec.endOfInput)

-- | Attempt to parse the standard file path which is an alias to 'String'.
maybeFromFilePath :: FilePath -> Maybe Path
maybeFromFilePath = maybeFromText . fromString

-- | Attoparsec parser of 'Path'.
attoparsecParserOf :: Attoparsec.Parser Path
attoparsecParserOf =
  fromAst <$> Ast.Path.attoparsecParserOf

-- | Construct from AST.
fromAst :: Ast.Path.Path -> Path
fromAst (Ast.Path.Path root components) =
  foldr step finish components 0 []
  where
    step component next !collectedMovesUp !collectedNames =
      case component of
        Ast.Component.NameComponent name ->
          if collectedMovesUp > 0
            then next (pred collectedMovesUp) collectedNames
            else
              if Ast.Name.null name
                then next collectedMovesUp collectedNames
                else next collectedMovesUp (name : collectedNames)
        Ast.Component.DotComponent ->
          next collectedMovesUp collectedNames
        Ast.Component.DotDotComponent ->
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
--
-- >>> root <> "a/b"
-- "/a/b"
root :: Path
root =
  AbsNormalizedPath []

-- * Traversers (aka Van Laarhoven lenses)

traverseNames :: (Functor f) => ([Ast.Name.Name] -> f [Ast.Name.Name]) -> Path -> f Path
traverseNames f = \case
  AbsNormalizedPath names ->
    AbsNormalizedPath <$> f names
  RelNormalizedPath movesUp names ->
    RelNormalizedPath movesUp <$> f names

traverseLastName :: (Functor f) => (Ast.Name.Name -> f Ast.Name.Name) -> Path -> f Path
traverseLastName =
  traverseNames . List.traverseHeadWithDefault Ast.Name.empty

traverseExtensions :: (Functor f) => ([Text] -> f [Text]) -> Path -> f Path
traverseExtensions =
  traverseLastName . Ast.Name.traverseExtensions

-- * Mappers

mapNames :: ([Ast.Name.Name] -> [Ast.Name.Name]) -> Path -> Path
mapNames f = \case
  AbsNormalizedPath names ->
    AbsNormalizedPath (f names)
  RelNormalizedPath movesUp names ->
    RelNormalizedPath movesUp (f names)

mapHeadName :: (Ast.Name.Name -> Ast.Name.Name) -> Path -> Path
mapHeadName f = mapNames $ \case
  head : tail -> f head : tail
  [] -> []

mapExtensions :: ([Text] -> [Text]) -> Path -> Path
mapExtensions mapper =
  runIdentity . traverseExtensions (Identity . mapper)

-- | Add file extension to the last segment of the path.
--
-- >>> addExtension "tar" "/a/b"
-- "/a/b.tar"
--
-- >>> addExtension "gz" "/a/b.tar"
-- "/a/b.tar.gz"
--
-- >>> addExtension "gitignore" ""
-- "./.gitignore"
--
-- Dots in the prefix are ignored:
--
-- >>> addExtension ".gitignore" ""
-- "./.gitignore"
--
-- In fact dots are interpreted as extension delimiters, meaning that the following two are equivalent:
--
-- >>> addExtension "tar.gz" ""
-- "./.tar.gz"
--
-- >>> (addExtension "gz" . addExtension "tar") ""
-- "./.tar.gz"
addExtension :: Text -> Path -> Path
addExtension ext = mapExtensions (ext :)

-- | Make the path non-absolute.
--
-- >>> deabsolutize "/a/b"
-- "./a/b"
--
-- >>> deabsolutize "a/b"
-- "./a/b"
deabsolutize :: Path -> Path
deabsolutize = \case
  AbsNormalizedPath names -> RelNormalizedPath 0 names
  relPath -> relPath

-- | Drop path to the parent directory.
--
-- >>> dropParent "/a/b"
-- "./b"
--
-- >>> dropParent "a/b"
-- "./b"
--
-- >>> dropParent "../a/b"
-- "./b"
--
-- >>> dropParent ".."
-- "."
--
-- >>> dropParent "."
-- "."
--
-- >>> dropParent "/"
-- "."
dropParent :: Path -> Path
dropParent = fromNames . toNames
  where
    fromNames = \case
      head : _ -> RelNormalizedPath 0 [head]
      _ -> RelNormalizedPath 0 []

-- | Drop the last segment of the path.
--
-- >>> dropLastSegment "./a/b.c.d"
-- "./a"
--
-- >>> dropLastSegment "./a"
-- "."
--
-- >>> dropLastSegment "."
-- "."
--
-- >>> dropLastSegment "/"
-- "/"
--
-- >>> dropLastSegment ".."
-- ".."
dropLastSegment :: Path -> Path
dropLastSegment =
  mapNames \case
    [] -> []
    _ : tail -> tail

-- | Drop last extension if there is one.
--
-- >>> dropExtension "/a/b.c.d"
-- "/a/b.c"
--
-- >>> dropExtension "/a/b.c"
-- "/a/b"
--
-- >>> dropExtension "/a/b"
-- "/a/b"
--
-- >>> dropExtension "/"
-- "/"
dropExtension :: Path -> Path
dropExtension = mapHeadName $ Ast.Name.mapExtensions $ List.drop 1

-- | Drop all extensions.
--
-- >>> dropExtensions "/a/b.c.d"
-- "/a/b"
--
-- >>> dropExtensions "a"
-- "./a"
dropExtensions :: Path -> Path
dropExtensions = mapHeadName $ Ast.Name.mapExtensions $ const []

-- * Accessors

-- | Compile to strict text builder.
toTextBuilder :: Path -> TextBuilder.TextBuilder
toTextBuilder = Ast.Path.toTextBuilder . toAst

-- | Compile to standard file path string.
toFilePath :: Path -> FilePath
toFilePath = toList . toText

-- | Compile to text.
toText :: Path -> Text
toText = TextBuilder.run . toTextBuilder

toAst :: Path -> Ast.Path.Path
toAst = \case
  AbsNormalizedPath names ->
    Ast.Path.Path True (fmap Ast.Component.NameComponent names)
  RelNormalizedPath movesUp names ->
    Ast.Path.Path False
      $ if movesUp == 0
        then
          fmap Ast.Component.NameComponent names
            <> pure Ast.Component.DotComponent
        else
          fmap Ast.Component.NameComponent names
            <> replicate movesUp Ast.Component.DotDotComponent

-- |
-- Decompose into individual segments as texts. I.e., the parts separated by slashes.
--
-- >>> toSegments "a/b.c.d"
-- ["a","b.c.d"]
--
-- >>> toSegments "/a/b.c.d"
-- ["a","b.c.d"]
--
-- >>> toSegments "."
-- []
--
-- >>> toSegments "/"
-- []
--
-- If you also want to retain the information on the path being absolute, use this in combination with 'isAbsolute':
--
-- >>> let path = "/a/b.c.d" :: Path
-- >>> toSegments path
-- ["a","b.c.d"]
-- >>> isAbsolute path
-- True
toSegments :: Path -> [Text]
toSegments = fmap Ast.Name.toText . reverse . toNames

toNames :: Path -> [Ast.Name.Name]
toNames = \case
  AbsNormalizedPath names -> names
  RelNormalizedPath _ names -> names

-- | Get the last segment sans extensions.
--
-- >>> toBasename "/a/b.c.d"
-- "b"
--
-- >>> toBasename "/a/b"
-- "b"
--
-- >>> toBasename "/a/.gitignore"
-- ""
toBasename :: Path -> Text
toBasename path =
  case toNames path of
    head : _ -> Ast.Name.toBase head
    _ -> mempty

-- | Convert to a list of extensions.
--
-- >>> toExtensions "/a/b.c.d"
-- ["c","d"]
--
-- >>> toExtensions "/a/b"
-- []
toExtensions :: Path -> [Text]
toExtensions =
  reverse . Ast.Name.toExtensions . List.headOr Ast.Name.empty . toNames

-- | Check if the path is absolute.
--
-- >>> isAbsolute "/a/b"
-- True
--
-- >>> isAbsolute "a/b"
-- False
--
-- >>> isAbsolute "/"
-- True
--
-- >>> isAbsolute "."
-- False
isAbsolute :: Path -> Bool
isAbsolute = \case
  AbsNormalizedPath _ -> True
  RelNormalizedPath _ _ -> False
