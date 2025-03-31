module PosixPathStructures.Path
  ( Path (..),
  )
where

import Coalmine.BaseExtras.MonadPlus
import Coalmine.Prelude hiding (Path, null)
import Coalmine.SyntaxModelling qualified as Syntax
import Data.Attoparsec.Text qualified as Attoparsec
import PosixPathStructures.Component qualified as Component
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
