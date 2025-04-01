module PosixPath.Ast.Path
  ( Path (..),
    attoparsecParserOf,
    toTextBuilder,
  )
where

import Data.Attoparsec.Text qualified as Attoparsec
import PosixPath.Ast.Component qualified as Component
import PosixPath.Util.MonadPlus
import TextBuilder qualified as TextBuilder
import Prelude

data Path
  = Path Bool [Component.Component]

attoparsecParserOf :: Attoparsec.Parser Path
attoparsecParserOf = do
  abs <- Attoparsec.char '/' $> True <|> pure False
  components <- reverseSepBy Component.attoparsecParserOf (Attoparsec.char '/')
  return $ Path abs components

toTextBuilder :: Path -> TextBuilder.TextBuilder
toTextBuilder (Path abs components) =
  if abs
    then "/" <> relative
    else relative
  where
    relative =
      TextBuilder.intercalate "/" . fmap Component.toTextBuilder . reverse $ components
