module PosixPathStructures.Path
  ( Path (..),
  )
where

import Coalmine.BaseExtras.List qualified as List
import Coalmine.BaseExtras.MonadPlus
import Coalmine.EvenSimplerPaths.AttoparsecHelpers qualified as AttoparsecHelpers
import Coalmine.EvenSimplerPaths.QuickCheckGens qualified as QuickCheckGens
import Coalmine.Prelude hiding (null, Path)
import PosixPathStructures.Component qualified as Component
import PosixPathStructures.Name qualified as Name
import Coalmine.SyntaxModelling qualified as Syntax
import Data.Attoparsec.Text qualified as Attoparsec
import Data.List qualified as List
import Data.Serialize qualified as Cereal
import Data.Text qualified as Text
import Test.QuickCheck qualified as QuickCheck
import TextBuilderDev qualified as TextBuilderDev

data Path
  = Path Bool [Component.Component]

instance Syntax.Syntax Path where
  attoparsecParser = do
    abs <- Attoparsec.char '/' $> True <|> pure False
    components <- reverseSepBy Syntax.attoparsecParser (Attoparsec.char '/')
    return $ Path abs components
  toTextBuilder (Path abs components) =
    if abs
      then "/" <> relative
      else relative
    where
      relative =
        TextBuilderDev.intercalate "/" . fmap Syntax.toTextBuilder . reverse $ components
