module Coalmine.XmlParser.Attoparsec where

import Coalmine.InternalPrelude
import Data.Attoparsec.Text

name :: (Text -> Maybe Text -> a) -> Parser a
name k =
  (k <$> ident <*> optional (char ':' >> ident))
    <?> "name"

ident :: Parser Text
ident = takeWhile1 $ \case
  '&' -> False
  '<' -> False
  '>' -> False
  ':' -> False
  '?' -> False
  '=' -> False
  '"' -> False
  '\'' -> False
  '/' -> False
  ';' -> False
  '#' -> False
  ' ' -> False
  '\t' -> False
  '\r' -> False
  '\n' -> False
  _ -> True
