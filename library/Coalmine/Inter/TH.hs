module Coalmine.Inter.TH where

import qualified Coalmine.Inter.Deindentation as D
import qualified Coalmine.MultilineTextBuilder as B
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
  AppE
    (AppE (VarE 'B.indent) (LitE (IntegerL (fromIntegral indent))))
    (VarE varName)
  where
    varName = mkName $ #head name : toString (#tail name)

literalExps :: [Text] -> [Exp]
literalExps literalChunks =
  case literalChunks of
    [] -> []
    _ -> pure $ Helpers.textLitE $ mconcat $ reverse literalChunks
