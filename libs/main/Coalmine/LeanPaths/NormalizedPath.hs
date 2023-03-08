module Coalmine.LeanPaths.NormalizedPath
  ( NormalizedPath (..),
    fromPath,
    toPath,
    absolute,
    decompose,
  )
where

import Coalmine.BaseExtras.List qualified as List
import Coalmine.BaseExtras.MonadPlus
import Coalmine.EvenSimplerPaths.AttoparsecHelpers qualified as AttoparsecHelpers
import Coalmine.EvenSimplerPaths.QuickCheckGens qualified as QuickCheckGens
import Coalmine.InternalPrelude hiding (null)
import Coalmine.LeanPaths.Component qualified as Component
import Coalmine.LeanPaths.Name qualified as Name
import Coalmine.LeanPaths.Path qualified as Path
import Coalmine.SyntaxModelling qualified as Syntax
import Data.Attoparsec.Text qualified as Attoparsec
import Data.List qualified as List
import Data.Serialize qualified as Cereal
import Data.Text qualified as Text
import Test.QuickCheck qualified as QuickCheck
import TextBuilderDev qualified as TextBuilderDev

-- |
-- Composable automatically normalized path.
data NormalizedPath
  = -- | Absolute path.
    AbsNormalizedPath
      ![Name.Name]
      -- ^ Components in reverse order.
  | RelNormalizedPath
      !Int
      -- ^ Preceding go up commands.
      ![Name.Name]
      -- ^ Components in reverse order.
  deriving (Eq, Show)

instance Ord NormalizedPath where
  compare = \case
    AbsNormalizedPath lNames -> \case
      AbsNormalizedPath rNames -> compare lNames rNames
      _ -> GT
    RelNormalizedPath lMovesUp lNames -> \case
      AbsNormalizedPath _ -> LT
      RelNormalizedPath rMovesUp rNames -> case compare lMovesUp rMovesUp of
        EQ -> compare lNames rNames
        res -> res

instance IsString NormalizedPath where
  fromString =
    either error id
      . Attoparsec.parseOnly (Syntax.attoparsec <* Attoparsec.endOfInput)
      . fromString

instance Semigroup NormalizedPath where
  lPath <> rPath = case rPath of
    AbsNormalizedPath _ -> rPath
    RelNormalizedPath rMovesUp rNames ->
      case lPath of
        RelNormalizedPath lMovesUp lNames -> case List.dropPedantically rMovesUp lNames of
          Left rMovesUp -> RelNormalizedPath (rMovesUp + lMovesUp) rNames
          Right lNames -> RelNormalizedPath lMovesUp (lNames <> rNames)

instance Monoid NormalizedPath where
  mempty =
    RelNormalizedPath 0 []

instance QuickCheck.Arbitrary NormalizedPath where
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
        QuickCheck.vectorOf size QuickCheck.arbitrary
  shrink = \case
    AbsNormalizedPath names ->
      AbsNormalizedPath <$> QuickCheck.shrink names
    RelNormalizedPath movesUp names ->
      QuickCheck.shrink (movesUp, names) <&> \(movesUp, names) ->
        RelNormalizedPath movesUp names

instance Cereal.Serialize NormalizedPath where
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

instance Syntax.Syntax NormalizedPath where
  attoparsec =
    fromPath <$> Syntax.attoparsec
  textBuilder =
    Syntax.textBuilder . toPath

-- |
-- Normalize a path.
fromPath :: Path.Path -> NormalizedPath
fromPath (Path.Path absolute components) =
  foldr step finish components 0 []
  where
    step component next !collectedMovesUp !collectedNames =
      case component of
        Component.NameComponent name ->
          if collectedMovesUp > 0
            then next (pred collectedMovesUp) collectedNames
            else
              if Name.null name
                then next collectedMovesUp collectedNames
                else next collectedMovesUp (name : collectedNames)
        Component.DotComponent ->
          next collectedMovesUp collectedNames
        Component.DotDotComponent ->
          next (succ collectedMovesUp) collectedNames
    finish collectedMovesUp collectedNames =
      if absolute
        then AbsNormalizedPath (reverse collectedNames)
        else RelNormalizedPath collectedMovesUp (reverse collectedNames)

toPath :: NormalizedPath -> Path.Path
toPath = \case
  AbsNormalizedPath names ->
    Path.Path True (fmap Component.NameComponent names)
  RelNormalizedPath movesUp names ->
    Path.Path False $
      if movesUp == 0
        then
          fmap Component.NameComponent names
            <> pure Component.DotComponent
        else
          fmap Component.NameComponent names
            <> replicate movesUp Component.DotDotComponent

-- |
-- Prepending it to a relative path will make it absolute.
absolute :: NormalizedPath
absolute =
  AbsNormalizedPath []

-- |
-- Explode into individual components.
decompose :: NormalizedPath -> [NormalizedPath]
decompose = \case
  AbsNormalizedPath names ->
    case reverse names of
      head : tail ->
        AbsNormalizedPath [head]
          : fmap (RelNormalizedPath 0 . pure) tail
      _ ->
        [AbsNormalizedPath []]
  RelNormalizedPath movesUp names ->
    replicate movesUp (RelNormalizedPath 1 [])
      <> fmap (RelNormalizedPath 0 . pure) (reverse names)
