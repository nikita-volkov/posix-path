{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module PosixPath
  ( Path,

    -- * Accessors
    toFilePath,
    toText,
    toBasename,
    toExtensions,

    -- * Constructors
    root,
    maybeFromText,
    maybeFromFilePath,

    -- * Algebra
    parent,
    sansParent,
    relativeTo,
  )
where

import Data.Serialize qualified as Cereal
import PosixPath.Structures.NormalizedPath qualified as NormalizedPath
import PosixPath.Util.Prelude

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
newtype Path = Path {underlying :: NormalizedPath.NormalizedPath}
  deriving newtype (Eq, Ord, Show, IsString, Semigroup, Monoid, Arbitrary, Cereal.Serialize, Hashable)

-- * Partial constructors

-- | Parse text.
maybeFromText :: Text -> Maybe Path
maybeFromText = fmap Path . NormalizedPath.maybeFromText

-- | Parse file path.
maybeFromFilePath :: FilePath -> Maybe Path
maybeFromFilePath = maybeFromText . fromString

-- * Constructors

-- |
-- Prepending it to a relative path will make it absolute.
root :: Path
root = Path NormalizedPath.root

-- * Accessors

-- | Compile to standard file path string.
toFilePath :: Path -> FilePath
toFilePath = NormalizedPath.toFilePath . coerce

-- | Compile to text.
toText :: Path -> Text
toText = NormalizedPath.toText . coerce

null :: Path -> Bool
null =
  error "TODO"

-- | Whether the path starts from root.
absolute :: Path -> Bool
absolute =
  error "TODO"

-- | File name sans toExtensions.
toBasename :: Path -> Text
toBasename = NormalizedPath.toBasename . coerce

toExtensions :: Path -> [Text]
toExtensions = NormalizedPath.toExtensions . coerce

-- * Algebra

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
parent = mapNormalizedPath NormalizedPath.parent

-- |
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
sansParent = mapNormalizedPath NormalizedPath.dropParent

sansExtension :: Path -> Path
sansExtension = mapNormalizedPath NormalizedPath.dropExtension

mapNormalizedPath ::
  (NormalizedPath.NormalizedPath -> NormalizedPath.NormalizedPath) ->
  Path ->
  Path
mapNormalizedPath f =
  Path . f . coerce

mapBasename :: (Text -> Text) -> Path -> Path
mapBasename =
  error "TODO"

mapExtensions :: ([Text] -> [Text]) -> Path -> Path
mapExtensions =
  error "TODO"

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
relativeTo :: Path -> Path -> Maybe Path
relativeTo = coerce NormalizedPath.relativeTo
