module Coalmine.MultilineTextBuilder where

import Coalmine.InternalPrelude
import Coalmine.Building
import qualified Text.Builder as Tb
import qualified Coalmine.List as List
import qualified Data.Text as Text


newtype Builder =
  Builder (Int -> Tb.Builder)
  deriving (Semigroup, Monoid)

instance Building Builder where
  type BuilderTarget Builder = Tb.Builder
  toBuilder = textBuilder
  fromBuilder = toTextBuilder

instance Show Builder where
  show = show . fromBuilder

instance IsString Builder where
  fromString = text . fromString

instance ToString Builder where
  toString = toString . toTextBuilder


-- * Execution
-------------------------

toTextBuilder :: Builder -> Tb.Builder
toTextBuilder (Builder builder) =
  builder 0


-- * Transformation
-------------------------

{-|

The way it works is by inserting indentation after each newline character.
This means that everything that precedes the first newline will not be indented.

>>> indent 2 ("abc\nd\nf" <> indent 2 ("\ng\nh" <> "i\nj"))
abc
  d
  f
    g
    hi
    j
>>> indent 2 ("\nabc\nd\nf" <> indent 2 ("\ng\nh" <> "\ni\nj"))
<BLANKLINE>
  abc
  d
  f
    g
    h
    i
    j
-}
indent :: Int -> Builder -> Builder
indent amount (Builder builder) =
  Builder (\outerAmount -> builder (amount + outerAmount))


-- * Construction
-------------------------

{-|
Same as @'text' . 'fromBuilder'@.

For details on its behavior refer to 'text'.
-}
textBuilder :: Tb.Builder -> Builder
textBuilder =
  text . fromBuilder

{-|
Extract lines from input text and indent each one of them according to the rendering settings.
-}
text :: Text -> Builder
text text =
  Builder impl
  where
    impl indentationAmount =
      List.foldMapHeadAndTail Tb.text (foldMap lineTextMapper) lines
      where
        lines =
          Text.split (== '\n') text
        indentationText =
          Text.replicate indentationAmount (Text.singleton ' ')
        linePrefixBuilder =
          Tb.char '\n' <> Tb.text indentationText
        lineTextMapper lineText =
          linePrefixBuilder <> Tb.text lineText
