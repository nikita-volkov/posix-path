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
  mconcatExp . build
  where
    build vec = foldr step (const []) vec True
    step line next firstLine =
      case line of
        D.BlankLine ->
          if firstLine
            then next False
            else unilineBuilder "\n" : next False
        D.ContentLine indentation segments ->
          lineExps firstLine indentation segments <> next False

-- *

lineExps firstLine indentation segments =
  foldr progress finish segments prefix
  where
    prefix =
      (if firstLine then id else ("\n" <>)) $
        fromText (Text.replicate indentation " ")
    progress segment next litBuilder =
      case segment of
        D.PlainContentSegment content ->
          next (litBuilder <> content)
        D.DollarContentSegment ->
          next (litBuilder <> "$")
        D.PlaceholderContentSegment name ->
          prependLitIfNeeded litBuilder $
            indent indentation (placeholder name) :
            next mempty
    finish litBuilder =
      prependLitIfNeeded litBuilder []
    prependLitIfNeeded litBuilder =
      if Text.null text
        then id
        else (unilineBuilder text :)
      where
        text = toText litBuilder

-- *

litBuilder :: Text -> Exp
litBuilder text =
  SigE
    (AppE (VarE 'fromString) (Helpers.textLitE text))
    (ConT ''B.Builder)

unilineBuilder :: Text -> Exp
unilineBuilder text =
  AppE (VarE 'B.uniline) (Helpers.textLitE text)

indent :: Int -> Exp -> Exp
indent indent =
  AppE (AppE (VarE 'B.indent) (LitE (IntegerL (fromIntegral indent))))

placeholder :: D.Name -> Exp
placeholder name =
  VarE $ mkName $ #head name : toString (#tail name)
