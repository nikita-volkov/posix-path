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
newtype Path = Path {underlying :: NormalizedPath.NormalizedPath}
  deriving newtype (Eq, Ord, IsString, Semigroup, Monoid, Arbitrary, Cereal.Serialize, Syntax.Syntax)

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

{-| Whether the path starts from root. -}
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
-- If the path is absolute and it points to root,
-- the same path will be returned:
-- 
-- >>> parent "/" == "/"
-- 
-- If the path is relative and is either empty or points outside,
-- another level up will be added.
-- 
-- >>> parent "." == ".."
-- 
-- >>> parent ".." == "../.."
-- 
-- For all other cases the behaviour should be self-evident:
-- 
-- >>> parent "/a/b" == "/a"
-- 
-- >>> parent "a/b" == "a"
-- 
-- >>> parent "a" == "."
-- 
-- >>> parent "../a" == ".."
parent :: Path -> Path
parent = mapNormalizedPath NormalizedPath.parent

-- |
-- >>> last "/a/b" == "b"
-- 
-- >>> last "a/b" == "b"
-- 
-- >>> last "../a/b" == "b"
-- 
-- >>> last "." == "."
last :: Path -> Path
last = mapNormalizedPath NormalizedPath.last

mapNormalizedPath ::
  (NormalizedPath.NormalizedPath -> NormalizedPath.NormalizedPath) -> 
  Path -> Path
mapNormalizedPath f = 
  Path . f . coerce

mapBasename :: (Text -> Text) -> Path -> Path
mapBasename =
  error "TODO"

mapExtensions :: ([Text] -> [Text]) -> Path -> Path
mapExtensions =
  error "TODO"
