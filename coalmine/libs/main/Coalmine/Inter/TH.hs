module Coalmine.Inter.TH where

import Coalmine.Inter.Deindentation qualified as D
import Coalmine.InternalPrelude
import Coalmine.MultilineTextBuilder qualified as B
import Coalmine.Printing
import Coalmine.TH.Exp
import Data.Text qualified as Text
import Language.Haskell.TH.Syntax
import THLego.Helpers qualified as Helpers

-- * --

toLinesExp :: BVec D.Line -> Exp
toLinesExp =
  AppE (VarE 'to) . linesExp

linesExp :: BVec D.Line -> Exp
linesExp =
  mconcatExp
    . build
  where
    build vec = foldr step (const []) vec True
    step line next firstLine =
      (if firstLine then [] else [newlineBuilder])
        <> lineExps line
        <> next False

-- * --

lineExps :: D.Line -> [Exp]
lineExps = \case
  D.BlankLine ->
    []
  D.ContentLine indentation segments ->
    foldr progress finish segments prefix
    where
      prefix =
        Text.replicate indentation " "
      progress segment next lit =
        case segment of
          D.PlainContentSegment content ->
            next (lit <> content)
          D.DollarContentSegment ->
            next (lit <> "$")
          D.PlaceholderContentSegment name ->
            prependLitIfNeeded lit
              $ indent indentation (placeholder name)
              : next mempty
      finish lit =
        prependLitIfNeeded lit []
      prependLitIfNeeded lit =
        if Text.null lit
          then id
          else (unilineBuilder lit :)

-- * --

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

placeholder :: NonEmpty D.Name -> Exp
placeholder (modelHead :| modelTail) =
  AppE (VarE 'toBroadBuilder)
    $ foldr
      ( \model next !basis ->
          next $ GetFieldE basis (nameString model)
      )
      id
      modelTail
      (name modelHead)

name :: D.Name -> Exp
name model =
  VarE $ mkName $ nameString model

-- * Non-exp

nameString :: D.Name -> String
nameString model =
  #head model : to @String (#tail model)
