module Coalmine.PosixPath
  ( Path,

    -- * Constructors
    root,

    -- * Accessors
    toFilePath,
    basename,
    extensions,
  )
where

import Coalmine.InternalPrelude
import Coalmine.PosixPath.Component qualified as Component
import Coalmine.PosixPath.Name qualified as Name
import Coalmine.PosixPath.NormalizedPath qualified as NormalizedPath
import Coalmine.PosixPath.Path qualified as Path
import Coalmine.SyntaxModelling qualified as Syntax
import Data.Serialize qualified as Cereal
import Test.QuickCheck qualified as QuickCheck

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
-- "a"
--
-- Multislash gets squashed:
--
-- >>> "a//b" :: Path
-- "a/b"
--
-- Dot-dot gets immediately applied:
--
-- >>> "a/../c" :: Path
-- "c"
--
-- Same does the single dot:
--
-- >>> "a/./c" :: Path
-- "a/c"
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
-- "a"
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
  deriving newtype (Eq, Ord, IsString, Semigroup, Monoid, Arbitrary, Cereal.Serialize, Syntax.Syntax, Hashable)

-- * Constructors

-- |
-- Prepending it to a relative path will make it absolute.
root :: Path
root = Path NormalizedPath.root

-- * Accessors

-- | Compile to standard file path string.
toFilePath :: Path -> FilePath
toFilePath = NormalizedPath.toFilePath . coerce

null :: Path -> Bool
null =
  error "TODO"

-- | Whether the path starts from root.
absolute :: Path -> Bool
absolute =
  error "TODO"

-- | File name sans extensions.
basename :: Path -> Text
basename = NormalizedPath.basename . coerce

extensions :: Path -> [Text]
extensions = NormalizedPath.extensions . coerce

-- * Mappers

-- | Get the parent directory.
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
-- >>> last "/a/b"
-- "b"
--
-- >>> last "a/b"
-- "b"
--
-- >>> last "../a/b"
-- "b"
--
-- >>> last "."
-- "."
last :: Path -> Path
last = mapNormalizedPath NormalizedPath.last

dropExtension :: Path -> Path
dropExtension = mapNormalizedPath NormalizedPath.dropExtension

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
