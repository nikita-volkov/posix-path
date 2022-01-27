module Coalmine.SimplePaths
  ( DirPath,
    FilePath,
    inDir,
  )
where

import Attoparsec.Data (LenientParser (..))
import Coalmine.Prelude hiding (FilePath)
import qualified Coalmine.SimplePaths.AttoparsecHelpers as AttoparsecHelpers
import qualified Data.Attoparsec.Text as Attoparsec
import qualified TextBuilder

-- *

-- |
-- >>> "a/b" :: DirPath
-- "a/b/"
--
-- >>> "a/b/" :: DirPath
-- "a/b/"
--
-- >>> "/a/b/" :: DirPath
-- "/a/b/"
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
    DirPath True <$> AttoparsecHelpers.absDirPath
      <|> DirPath False <$> AttoparsecHelpers.dirPath

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
      then "/" <> foldMap (flip mappend "/" . fromText) dirs
      else foldMap (flip mappend "/" . fromText) dirs

instance Show DirPath where
  show = show . toText

-- *

-- |
-- >>> inDir "a/b" ("c/d/e" :: FilePath)
-- "a/b/c/d/e"
--
-- >>> inDir "a/b/" ("c/d/e" :: FilePath)
-- "a/b/c/d/e"
--
-- >>> inDir "a/b/" ("/c/d/e" :: FilePath)
-- "/c/d/e"
data FilePath
  = FilePath
      !DirPath
      -- ^ Directories.
      !Text
      -- ^ File name.
      ![Text]
      -- ^ File extensions.

instance LenientParser FilePath where
  lenientParser =
    FilePath <$> dir <*> AttoparsecHelpers.fileName <*> many AttoparsecHelpers.extension
    where
      dir = DirPath <$> AttoparsecHelpers.abs <*> AttoparsecHelpers.filePathDirs

instance IsString FilePath where
  fromString =
    either error id
      . Attoparsec.parseOnly (AttoparsecHelpers.complete lenientParser)
      . fromString

instance ToString FilePath where
  toString = toString . toText

instance ToText FilePath where
  toText = toText . toTextBuilder

instance ToTextBuilder FilePath where
  toTextBuilder (FilePath dir name extensions) =
    toTextBuilder dir <> toTextBuilder name
      <> foldMap (mappend "." . toTextBuilder) extensions

instance Show FilePath where
  show = show . toText

-- *

class InDir path where
  inDir :: DirPath -> path -> path

instance InDir FilePath where
  inDir lDir (FilePath rDir name extensions) =
    FilePath (lDir <> rDir) name extensions

instance InDir DirPath where
  inDir = (<>)

-- *

mapFileName :: (Text -> Text) -> FilePath -> FilePath
mapFileName = error "TODO"

mapFileExtensions :: ([Text] -> [Text]) -> FilePath -> FilePath
mapFileExtensions = error "TODO"
