module Coalmine.Inter.Format.Parsers where

import Coalmine.Inter.Format.Model
import Coalmine.Prelude hiding (takeWhile)
import Data.Attoparsec.Text
import qualified VectorExtras.Combinators as VectorCombinators

quasiQuote :: Parser QuasiQuote
quasiQuote =
  do
    spaces
    endOfLine
    QuasiQuote <$> content
  where
    content =
      VectorCombinators.sepEnd sep end line
      where
        sep = endOfLine
        end = endOfLine >> spaces >> endOfInput

spaces :: Parser (BVec Space)
spaces =
  VectorCombinators.many space
  where
    space =
      asum
        [ SpaceSpace <$ char ' ',
          TabSpace <$ char '\t'
        ]

line :: Parser Line
line =
  Line <$> spaces <*> VectorCombinators.many contentSegment

contentSegment :: Parser ContentSegment
contentSegment =
  asum
    [ PlainContentSegment <$> takeWhile1 isPlainContentChar,
      VLineContentSegment <$ string "||",
      PlaceholderContentSegment <$> placeholder
    ]
  where
    isPlainContentChar x =
      x /= '\n' && x /= '\r' && x /= '|'
    placeholder =
      char '|' *> name <* char '|'

name :: Parser Name
name =
  Name <$> head <*> tail
  where
    head = satisfy $ and . applyAll [isAlpha, isLower]
    tail = takeWhile $ or . applyAll [isAlphaNum, (== '_'), (== '\'')]
