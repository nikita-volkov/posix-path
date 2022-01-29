module Coalmine.Inter.TH where

import qualified Coalmine.Inter.Deindentation as D
import qualified Coalmine.MultilineTextBuilder as B
import Coalmine.Prelude
import Coalmine.TH
import qualified Data.Text as Text
import Language.Haskell.TH.Syntax
import qualified THLego.Helpers as Helpers

-- *

linesExp :: BVec D.Line -> Exp
linesExp =
  mconcatExp . \vec -> foldr progress finish vec True 0 []
  where
    progress line next !first !indent !literalChunks =
      case line of
        D.BlankLine -> next False 0 prelinedLiteralChunks
        D.ContentLine indent segments ->
          foldr progress (next False) segments indent (Text.replicate indent " " : prelinedLiteralChunks)
          where
            progress segment next !indent !literalChunks =
              case segment of
                D.PlainContentSegment content ->
                  next indent (content : literalChunks)
                D.VLineContentSegment ->
                  next indent ("|" : literalChunks)
                D.PlaceholderContentSegment placeholderName ->
                  literalExps literalChunks
                    <> (placeholder indent placeholderName : next indent [])
      where
        prelinedLiteralChunks =
          if first then literalChunks else "\n" : literalChunks
    finish _ !indent !literalChunks =
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
