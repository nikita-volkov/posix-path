module Coalmine.NormalizedPath
  ( -- * --
    Path,
  )
where

import Algorithms.NaturalSort qualified as NaturalSort
import Coalmine.BaseExtras.List qualified as List
import Coalmine.BaseExtras.MonadPlus
import Coalmine.CerealExtras.Compact qualified as CerealExtrasCompact
import Coalmine.EvenSimplerPaths.AttoparsecHelpers qualified as AttoparsecHelpers
import Coalmine.EvenSimplerPaths.IsomorphismClassHelpers qualified as IsomorphismClassHelpers
import Coalmine.EvenSimplerPaths.QuickCheckGens qualified as QuickCheckGens
import Coalmine.InternalPrelude
import Coalmine.NameConversion
import Coalmine.Printing
import Coalmine.SyntaxModelling qualified as Syntax
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Serialize qualified as Cereal
import Data.Text qualified as Text
import System.Directory qualified as Directory
import Test.QuickCheck qualified as QuickCheck
import TextBuilderDev qualified as TextBuilderDev

-- * --

-- |
-- Structured name of a single component of a path.
data Component = Component
  { -- | Name.
    name :: !Text,
    -- | Extensions in reverse order.
    extensions :: ![Text]
  }
  deriving (Eq)

instance QuickCheck.Arbitrary Component where
  arbitrary = do
    name <- QuickCheckGens.fileName
    fileExtensions <- QuickCheckGens.fileExtensions
    return $ Component name fileExtensions
  shrink (Component name extensions) =
    QuickCheck.shrink (name, extensions) <&> \(name, extensions) ->
      Component name extensions

instance Cereal.Serialize Component where
  put (Component name extensions) = do
    Cereal.put $ CerealExtrasCompact.Compact name
    Cereal.put $ CerealExtrasCompact.Compact $ fmap CerealExtrasCompact.Compact extensions
  get = do
    CerealExtrasCompact.Compact name <- Cereal.get
    CerealExtrasCompact.Compact extensions <- Cereal.get
    return $ Component name (fmap CerealExtrasCompact.unwrap extensions)

instance Ord Component where
  compare l r =
    if la == ra
      then
        if lb < rb
          then LT
          else
            if lb == rb
              then EQ
              else GT
      else
        if la < ra
          then LT
          else GT
    where
      la = componentNameSortKey l
      lb = componentExtensionsSortKey l
      ra = componentNameSortKey r
      rb = componentExtensionsSortKey r

componentNameSortKey = NaturalSort.sortKey . (.name)

componentExtensionsSortKey = reverse . fmap NaturalSort.sortKey . (.extensions)

-- * --

-- |
-- Composable automatically normalized path.
data Path
  = -- | Components in reverse order.
    AbsPath
      ![Component]
  | RelPath
      !Int
      -- ^ Preceding go up commands.
      ![Component]
      -- ^ Components in reverse order.
  deriving (Eq)

instance Ord Path where
  compare = \case
    AbsPath lComponents -> \case
      AbsPath rComponents -> compare lComponents rComponents
      _ -> GT
    RelPath lGoUps lComponents -> \case
      AbsPath _ -> LT
      RelPath rGoUps rComponents -> case compare lGoUps rGoUps of
        EQ -> compare lComponents rComponents
        res -> res

instance Show Path where
  show =
    show . to @Text . Syntax.textBuilder

instance IsString Path where
  fromString =
    either error id
      . Attoparsec.parseOnly (Syntax.attoparsec <* Attoparsec.endOfInput)
      . fromString

instance Semigroup Path where
  lPath <> rPath = case rPath of
    AbsPath _ -> rPath
    RelPath rGoUps rComps ->
      case lPath of
        RelPath lGoUps lComps -> case List.dropPedantically rGoUps lComps of
          Left rGoUps -> RelPath (rGoUps + lGoUps) rComps
          Right lComps -> RelPath lGoUps (lComps <> rComps)

instance Monoid Path where
  mempty =
    RelPath 0 []

instance QuickCheck.Arbitrary Path where
  arbitrary =
    QuickCheck.oneof [abs, rel]
    where
      abs =
        AbsPath <$> components
      rel =
        RelPath <$> goUps <*> components
        where
          goUps =
            QuickCheck.chooseInt (0, 3)
      components = do
        size <- QuickCheck.chooseInt (0, 20)
        QuickCheck.vectorOf size QuickCheck.arbitrary
  shrink = \case
    AbsPath components ->
      AbsPath <$> QuickCheck.shrink components
    RelPath goUps components ->
      RelPath goUps <$> QuickCheck.shrink components

instance Cereal.Serialize Path where
  put = \case
    AbsPath components -> do
      Cereal.put @Word8 0
      Cereal.put $ CerealExtrasCompact.Compact components
    RelPath goUps components -> do
      Cereal.put @Word8 1
      Cereal.put $ CerealExtrasCompact.Compact goUps
      Cereal.put $ CerealExtrasCompact.Compact components
  get = do
    tag <- Cereal.get @Word8
    case tag of
      0 -> do
        CerealExtrasCompact.Compact components <- Cereal.get
        return $ AbsPath components
      1 -> do
        CerealExtrasCompact.Compact goUps <- Cereal.get
        CerealExtrasCompact.Compact components <- Cereal.get
        return $ RelPath goUps components
      _ -> fail $ "Invalid tag: " <> show tag

instance Syntax.Syntax Path where
  attoparsec =
    error "TODO"
  textBuilder =
    error "TODO"
