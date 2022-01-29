module Coalmine.Inter.TH where

import qualified Coalmine.Inter.Deindentation as D
import Coalmine.Prelude
import qualified Data.Text as Text
import Language.Haskell.TH.Syntax
import qualified THLego.Helpers as Helpers

-- *

linesExpressions :: BVec D.Line -> [Exp]
linesExpressions =
  \vec -> foldr progress finish vec 0 []
  where
    progress line next !indent !literalChunks = case line of
      D.BlankLine -> next 0 ("\n" : literalChunks)
      D.ContentLine indent segments ->
        foldr progress next segments indent [Text.replicate indent " "]
        where
          progress segment next !indent !literalChunks =
            case segment of
              D.PlainContentSegment content ->
                next indent (content : literalChunks)
              D.VLineContentSegment ->
                next indent ("|" : literalChunks)
              D.PlaceholderContentSegment placeholderName ->
                placeholder indent placeholderName :
                literalExps literalChunks <> next indent []
    finish !indent !literalChunks =
      literalExps literalChunks

placeholder :: Int -> D.Name -> Exp
placeholder indent name =
  error "TODO"

literalExps :: [Text] -> [Exp]
literalExps literalChunks =
  case literalChunks of
    [] -> []
    _ -> pure $ Helpers.textLitE $ mconcat $ reverse literalChunks
