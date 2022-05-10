module Coalmine.SimplePaths
  ( DirPath,
    FilePath,
    inDir,
    filePathDir,
    filePathName,
    filePathExtensions,
    appendFileExtension,
  )
where

import Coalmine.InternalPrelude hiding (FilePath)
import Coalmine.Name (FromNameInSpinalCase (..), FromNameInUpperCamelCase (..))
import Coalmine.Printing
import qualified Coalmine.SimplePaths.AttoparsecHelpers as AttoparsecHelpers
import qualified Data.Attoparsec.Text as Attoparsec
import qualified TextBuilderDev as TextBuilder

-- * --

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
  deriving (Eq, Ord)

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

instance CompactPrinting DirPath where
  toCompactBuilder (DirPath abs dirs) =
    if abs
      then "/" <> foldMap (flip mappend "/" . to) dirs
      else foldMap (flip mappend "/" . to) dirs

instance PrettyPrinting DirPath where
  toPrettyBuilder =
    fromTextBuilder . toCompactBuilder

instance Show DirPath where
  show = show . printCompactAsText

instance ToJSON DirPath where
  toJSON = toJSON . printCompactAsText

instance ToJSONKey DirPath where
  toJSONKey = contramap printCompactAsText toJSONKey

instance FromNameInSpinalCase DirPath where
  fromNameInSpinalCase _name =
    DirPath False [fromNameInSpinalCase _name]

instance FromNameInUpperCamelCase DirPath where
  fromNameInUpperCamelCase _name =
    DirPath False [fromNameInUpperCamelCase _name]

-- * --

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
  deriving (Eq, Ord)

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

instance CompactPrinting FilePath where
  toCompactBuilder (FilePath dir name extensions) =
    toCompactBuilder dir <> from name
      <> foldMap (mappend "." . from) extensions

instance PrettyPrinting FilePath where
  toPrettyBuilder =
    fromTextBuilder . toCompactBuilder

instance Show FilePath where
  show = show . printCompactAsText

instance ToJSON FilePath where
  toJSON = toJSON . to @Text . toCompactBuilder

instance ToJSONKey FilePath where
  toJSONKey = contramap (to @Text . toCompactBuilder) toJSONKey

instance FromNameInSpinalCase FilePath where
  fromNameInSpinalCase _name =
    FilePath mempty (fromNameInSpinalCase _name) []

instance FromNameInUpperCamelCase FilePath where
  fromNameInUpperCamelCase _name =
    FilePath mempty (fromNameInUpperCamelCase _name) []

-- * --

class InDir path where
  inDir :: DirPath -> path -> path

instance InDir FilePath where
  inDir dir = mapFileDir (mappend dir)

instance InDir DirPath where
  inDir = (<>)

-- * --

mapFileDir :: (DirPath -> DirPath) -> FilePath -> FilePath
mapFileDir fn (FilePath dir name extensions) =
  FilePath (fn dir) name extensions

mapFileName :: (Text -> Text) -> FilePath -> FilePath
mapFileName fn (FilePath dir name exts) =
  FilePath dir (fn name) exts

mapFileExtensions :: ([Text] -> [Text]) -> FilePath -> FilePath
mapFileExtensions fn (FilePath dir name exts) =
  FilePath dir name (fn exts)

filePathDir :: FilePath -> DirPath
filePathDir (FilePath dir _ _) = dir

filePathName :: FilePath -> Text
filePathName (FilePath _ name _) = name

filePathExtensions :: FilePath -> [Text]
filePathExtensions (FilePath _ _ x) = x

appendFileExtension :: Text -> FilePath -> FilePath
appendFileExtension ext =
  mapFileExtensions (<> [ext])
