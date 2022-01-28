module Coalmine.MultilineTextBuilder
  ( -- *
    Builder,

    -- *
    ToMultilineTextBuilder (..),

    -- *
    null,
    indent,
    intercalate,
    textBuilderLine,
  )
where

import Coalmine.Building
import Coalmine.InternalPrelude hiding (intercalate, null)
import qualified Coalmine.List as List
import qualified Data.Text as Text
import qualified TextBuilder as Tb

-- *

class ToMultilineTextBuilder a where
  toMultilineTextBuilder :: a -> Builder

instance ToMultilineTextBuilder Builder where
  toMultilineTextBuilder = id

instance ToMultilineTextBuilder TextBuilder where
  toMultilineTextBuilder = text . fromBuilder

instance ToMultilineTextBuilder Text where
  toMultilineTextBuilder = text

instance ToMultilineTextBuilder String where
  toMultilineTextBuilder = fromString

-- *

data Builder
  = Builder Bool (Int -> Tb.TextBuilder)

instance Semigroup Builder where
  (<>) (Builder a b) (Builder c d) = Builder (a && c) (b <> d)

instance Monoid Builder where
  mempty = Builder True mempty

instance Building Builder where
  type BuilderTarget Builder = Tb.TextBuilder
  toBuilder = text . fromBuilder
  fromBuilder = toTextBuilder

instance Show Builder where
  show = show . fromBuilder

instance IsString Builder where
  fromString = text . fromString

instance ToString Builder where
  toString = toString . toTextBuilder

instance FromText Builder where
  fromText = text

instance ToText Builder where
  toText = toText . toTextBuilder

instance ToTextBuilder Builder where
  toTextBuilder (Builder _ builder) =
    builder 0

-- * Execution

null :: Builder -> Bool
null (Builder a _) =
  a

-- * Transformation

mapBuilder :: ((Int -> Tb.TextBuilder) -> Int -> Tb.TextBuilder) -> Builder -> Builder
mapBuilder mapper (Builder a b) =
  Builder a (mapper b)

-- |
--
-- The way it works is by inserting indentation after each newline character.
-- This means that everything that precedes the first newline will not be indented.
--
-- >>> indent 2 ("abc\nd\nf" <> indent 2 ("\ng\nh" <> "i\nj"))
-- abc
--   d
--   f
--     g
--     hi
--     j
-- >>> indent 2 ("\nabc\nd\nf" <> indent 2 ("\ng\nh" <> "\ni\nj"))
-- <BLANKLINE>
--   abc
--   d
--   f
--     g
--     h
--     i
--     j
indent :: Int -> Builder -> Builder
indent amount =
  mapBuilder (\builder outerAmount -> builder (amount + outerAmount))

-- |
-- Concatenate a list by inserting a separator between each element.
intercalate :: Builder -> [Builder] -> Builder
intercalate (Builder _sepNull _sepRender) _builders =
  Builder _null _render
  where
    _null = _sepNull && all null _builders
    _render _indent =
      Tb.intercalate
        (_sepRender _indent)
        (fmap (\(Builder _ _render) -> _render _indent) _builders)

-- * Construction

-- |
-- Extract lines from input text and indent each one of them according to the rendering settings.
text :: Text -> Builder
text text =
  Builder (Text.null text) impl
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

-- |
-- Efficiently lift a text builder, interpreting its contents as of a single line.
-- So if it contains newline characters, the contained lines won't be indented.
textBuilderLine :: Tb.TextBuilder -> Builder
textBuilderLine builder =
  Builder (Tb.null builder) $ \indentation ->
    let indentationText =
          Text.replicate indentation (Text.singleton ' ')
     in fromText indentationText <> builder
