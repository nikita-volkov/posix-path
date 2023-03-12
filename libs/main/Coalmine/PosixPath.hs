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

-- | File name sans extensions.
basename :: Path -> Text
basename = NormalizedPath.basename . coerce

extensions :: Path -> [Text]
extensions = NormalizedPath.extensions . coerce
