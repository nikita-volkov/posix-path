module Coalmine.SimplePaths
  ( DirPath,
    FilePath,
  )
where

import Attoparsec.Data (LenientParser (..))
import Coalmine.Prelude hiding (FilePath)
import qualified Coalmine.SimplePaths.AttoparsecHelpers as AttoparsecHelpers
import qualified Data.Attoparsec.Text as Attoparsec
import qualified TextBuilder

-- *

data DirPath
  = DirPath !Bool ![Text]

instance Semigroup DirPath where
  DirPath lAbs lDirs <> DirPath rAbs rDirs =
    if rAbs
      then DirPath True rDirs
      else DirPath lAbs (lDirs <> rDirs)

instance Monoid DirPath where
  mempty = DirPath False []

instance LenientParser DirPath where
  lenientParser =
    AttoparsecHelpers.directories <&> \case
      "" : directories -> DirPath True directories
      directories -> DirPath False directories

instance IsString DirPath where
  fromString =
    either error id
      . Attoparsec.parseOnly (AttoparsecHelpers.complete lenientParser)
      . fromString

instance ToString DirPath where
  toString = toString . toText

instance ToText DirPath where
  toText = toText . toTextBuilder

instance ToTextBuilder DirPath where
  toTextBuilder (DirPath abs dirs) =
    if abs
      then foldMap (mappend "/" . fromText) dirs
      else TextBuilder.intercalate "/" (fmap fromText dirs)

-- *

data FilePath
  = FilePath
      !DirPath
      -- ^ Directories.
      !Text
      -- ^ File name.
      ![Text]
      -- ^ File extensions.

-- *

class InDir path where
  inDir :: DirPath -> path -> path

instance InDir FilePath where
  inDir lDir (FilePath rDir fileName extensions) =
    FilePath (lDir <> rDir) fileName extensions

-- *

mapFileName :: (Text -> Text) -> FilePath -> FilePath
mapFileName = error "TODO"

mapFileExtensions :: ([Text] -> [Text]) -> FilePath -> FilePath
mapFileExtensions = error "TODO"
