module Coalmine.XmlSyntaxParser.Attoparsec where

import Coalmine.InternalPrelude
import Data.Attoparsec.Text

name :: (Maybe Text -> Text -> a) -> Parser a
name pack = labeled "ident" $ do
  a <- ident
  colon <- char ':' $> True <|> pure False
  if colon
    then do
      b <- ident
      return $ pack (Just a) b
    else return $ pack Nothing a

ident :: Parser Text
ident = labeled "ident" $ takeWhile1 $ \case
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

labeled :: String -> Parser a -> Parser a
labeled l p = p <?> l
