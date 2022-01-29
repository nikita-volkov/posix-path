module Coalmine.Inter.TH where

import qualified Coalmine.Inter.Normalization as N
import Coalmine.Prelude
import qualified Data.Text as Text
import Language.Haskell.TH.Syntax
import qualified THLego.Helpers as Helpers

-- *

linesExpressions :: BVec N.Line -> [Exp]
linesExpressions =
  \vec -> foldr progress finish vec 0 []
  where
    progress line next !indent !literalChunks = case line of
      N.BlankLine -> next 0 ("\n" : literalChunks)
      N.ContentLine indent segments ->
        foldr progress next segments indent [Text.replicate indent " "]
        where
          progress segment next !indent !literalChunks =
            case segment of
              N.PlainContentSegment content ->
                next indent (content : literalChunks)
              N.VLineContentSegment ->
                next indent ("|" : literalChunks)
              N.PlaceholderContentSegment placeholderName ->
                placeholder indent placeholderName :
                literalExps literalChunks <> next indent []
    finish !indent !literalChunks =
      literalExps literalChunks

placeholder :: Int -> N.Name -> Exp
placeholder = error "TODO"

literalExps :: [Text] -> [Exp]
literalExps literalChunks =
  case literalChunks of
    [] -> []
    _ -> pure $ Helpers.textLitE $ mconcat $ reverse literalChunks
