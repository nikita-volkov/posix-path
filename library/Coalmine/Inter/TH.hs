module Coalmine.Inter.TH where

import qualified Coalmine.Inter.Deindentation as D
import Coalmine.InternalPrelude
import qualified Coalmine.MultilineTextBuilder as B
import Coalmine.StringIsomorphism
import Coalmine.TH
import Coalmine.TextIsomorphism
import qualified Data.Text as Text
import Language.Haskell.TH.Syntax
import qualified THLego.Helpers as Helpers

-- *

linesExp :: BVec D.Line -> Exp
linesExp =
  AppE (VarE 'B.fromMultilineTextBuilder)
    . mconcatExp
    . build
  where
    build vec = foldr step (const []) vec True
    step line next firstLine =
      (if firstLine then [] else [newlineBuilder])
        <> lineExps line
        <> next False

-- *

lineExps = \case
  D.BlankLine ->
    []
  D.ContentLine indentation segments ->
    foldr progress finish segments prefix
    where
      prefix =
        fromText (Text.replicate indentation " ")
      progress segment next lit =
        case segment of
          D.PlainContentSegment content ->
            next (lit <> content)
          D.DollarContentSegment ->
            next (lit <> "$")
          D.PlaceholderContentSegment name ->
            prependLitIfNeeded lit $
              indent indentation (placeholder name) :
              next mempty
      finish lit =
        prependLitIfNeeded lit []
      prependLitIfNeeded lit =
        if Text.null text
          then id
          else (unilineBuilder text :)
        where
          text = toText lit

-- *

litBuilder :: Text -> Exp
litBuilder text =
  SigE
    (AppE (VarE 'fromString) (Helpers.textLitE text))
    (ConT ''B.Builder)

unilineBuilder :: Text -> Exp
unilineBuilder text =
  AppE (VarE 'B.uniline) (Helpers.textLitE text)

newlineBuilder :: Exp
newlineBuilder =
  VarE 'B.newline

indent :: Int -> Exp -> Exp
indent indent =
  AppE (AppE (VarE 'B.indent) (LitE (IntegerL (fromIntegral indent))))

placeholder :: D.Name -> Exp
placeholder name =
  AppE (VarE 'B.toMultilineTextBuilder) $
    VarE $ mkName $ #head name : toString (#tail name)
