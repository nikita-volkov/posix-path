module Coalmine.EvenSimplerPaths
  ( -- * --
    Path,

    -- * --
    toString,
    parent,
    components,
    extensions,

    -- * --
    createDirsTo,
    listDirectory,

    -- * --
    addExtension,
  )
where

import Coalmine.BaseExtras.MonadPlus
import qualified Coalmine.CerealExtras.Compact as CerealExtrasCompact
import qualified Coalmine.EvenSimplerPaths.AttoparsecHelpers as AttoparsecHelpers
import qualified Coalmine.EvenSimplerPaths.IsomorphismClassHelpers as IsomorphismClassHelpers
import qualified Coalmine.EvenSimplerPaths.QuickCheckGens as QuickCheckGens
import Coalmine.InternalPrelude
import qualified Coalmine.Name as Name
import Coalmine.NameConversion
import Coalmine.Printing
import qualified Coalmine.SimplePaths as SimplePaths
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Serialize as Cereal
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilderDev as TextBuilderDev

-- * --

-- |
-- Structured name of a single component of a path.
data Component
  = Component
      !Text
      -- ^ Name.
      ![Text]
      -- ^ Extensions in reverse order.
  deriving (Eq)

instance QuickCheck.Arbitrary Component where
  arbitrary = do
    name <- QuickCheckGens.fileName
    extensions <- QuickCheckGens.extensions
    return $ Component name extensions

instance Cereal.Serialize Component where
  put (Component name extensions) = do
    Cereal.put $ CerealExtrasCompact.Compact name
    Cereal.put $ CerealExtrasCompact.Compact $ fmap CerealExtrasCompact.Compact extensions
  get = do
    CerealExtrasCompact.Compact name <- Cereal.get
    CerealExtrasCompact.Compact extensions <- Cereal.get
    return $ Component name (fmap CerealExtrasCompact.unwrap extensions)

instance Ord Component where
  Component la lb <= Component ra rb =
    la <= ra || reverse lb <= reverse rb

-- * --

data Path
  = Path
      !Bool
      -- ^ Is it absolute?
      ![Component]
      -- ^ Components in reverse order.
  deriving (Eq)

instance QuickCheck.Arbitrary Path where
  arbitrary = Path <$> QuickCheck.arbitrary <*> components
    where
      components = do
        size <- QuickCheck.chooseInt (0, 20)
        QuickCheck.vectorOf size QuickCheck.arbitrary

instance Cereal.Serialize Path where
  put (Path abs components) = do
    Cereal.put abs
    Cereal.put $ CerealExtrasCompact.Compact components
  get = do
    abs <- Cereal.get
    CerealExtrasCompact.Compact components <- Cereal.get
    return $ Path abs components

instance Ord Path where
  Path la lb <= Path ra rb =
    la <= ra || reverse lb <= reverse rb

instance Semigroup Path where
  Path _lAbs _lNames <> Path _rAbs _rNames =
    if _rAbs
      then Path _rAbs _rNames
      else Path _lAbs $ _rNames <> _lNames

instance Monoid Path where
  mempty =
    Path False []

instance CompactPrinting Path where
  toCompactBuilder (Path _abs _components) =
    if _abs
      then "/" <> _relative
      else _relative
    where
      _relative =
        TextBuilderDev.intercalate "/" . fmap _fromComponent . reverse $ _components
      _fromComponent (Component _name _extensions) =
        foldl'
          (\_output _extension -> _output <> "." <> to _extension)
          (to _name)
          (reverse _extensions)

instance BroadPrinting Path where
  toBroadBuilder = to . toCompactBuilder

instance ToJSON Path where
  toJSON = toJSON . printCompactAsText

instance ToJSONKey Path where
  toJSONKey = contramap printCompactAsText toJSONKey

instance Show Path where
  show = show . printCompactAsText

instance LenientParser Path where
  lenientParser = do
    _abs <- Attoparsec.char '/' $> True <|> pure False
    _components <-
      catMaybes
        <$> reverseSepBy _componentOrDot (Attoparsec.char '/')
    optional $ Attoparsec.char '/'
    return $ Path _abs _components
    where
      _componentOrDot = do
        _baseName <- AttoparsecHelpers.fileName
        _extensions <- reverseMany AttoparsecHelpers.extension
        if Text.null _baseName && null _extensions
          then Nothing <$ Attoparsec.char '.' <|> pure Nothing
          else return $ Just $ Component _baseName _extensions

instance IsString Path where
  fromString =
    either error id
      . Attoparsec.parseOnly (lenientParser <* Attoparsec.endOfInput)
      . fromString

instance IsomorphicTo SimplePaths.DirPath Path where
  to = IsomorphismClassHelpers.thruText

instance IsomorphicTo Path SimplePaths.DirPath where
  to = IsomorphismClassHelpers.thruText

instance IsomorphicTo SimplePaths.FilePath Path where
  to = IsomorphismClassHelpers.thruText

instance IsomorphicTo Path SimplePaths.FilePath where
  to = IsomorphismClassHelpers.thruText

instance Name.FromNameInSpinalCase Path where
  fromNameInSpinalCase name =
    Path False [Component (Name.fromNameInSpinalCase name) []]

instance FromName Path where
  fromNameIn casing name =
    Path False [Component (fromNameIn casing name) []]

-- * --

-- | Helper for dealing with APIs for FilePath from base.
toString :: Path -> String
toString = to . toCompactBuilder

parent :: Path -> Maybe Path
parent (Path abs components) =
  case components of
    h : t -> Just $ Path abs t
    _ -> Nothing

-- | Decompose into components.
components :: Path -> [Path]
components (Path abs components) =
  if abs
    then case revComponents of
      h : t -> Path True [h] : nonAbsFromComponents t
      _ -> [Path True []]
    else nonAbsFromComponents revComponents
  where
    revComponents = reverse components
    nonAbsFromComponents = \case
      h : t -> Path False [h] : nonAbsFromComponents t
      _ -> []

extensions :: Path -> [Text]
extensions (Path _ components) =
  case components of
    Component _ extensions : _ -> reverse extensions
    [] -> []

-- * --

createDirsTo :: Path -> IO ()
createDirsTo =
  traverse_
    (Directory.createDirectoryIfMissing True . toString)
    . parent

listDirectory :: Path -> IO [Path]
listDirectory dir =
  Directory.listDirectory (printCompactAs dir)
    <&> fmap (mappend dir . fromString)

-- * Traversers (or Van Laarhoven lenses)

traverseLastComponent :: Functor f => (Component -> f Component) -> Path -> f Path
traverseLastComponent traverser (Path abs components) =
  case components of
    h : t -> traverser h <&> \h -> Path abs (h : t)
    _ -> traverser (Component "" []) <&> \h -> Path abs [h]

traverseExtensions :: Functor f => ([Text] -> f [Text]) -> Path -> f Path
traverseExtensions traverser =
  traverseLastComponent $ \(Component name extensions) ->
    traverser extensions <&> \extensions ->
      Component name extensions

-- * Mappers

mapExtensions :: ([Text] -> [Text]) -> Path -> Path
mapExtensions mapper =
  runIdentity . traverseExtensions (Identity . mapper)

-- * Editors

-- | Add file extension to the last component of the path.
addExtension :: Text -> Path -> Path
addExtension ext = mapExtensions (ext :)
