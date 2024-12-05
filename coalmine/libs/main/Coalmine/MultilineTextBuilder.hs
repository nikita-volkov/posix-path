module Coalmine.MultilineTextBuilder
  ( -- * --
    TextBlock,
    Splice,
    MultilineTextBuilder,
    Builder,

    -- * --
    null,
    ifNotNull,
    indent,
    prefixEachLine,
    intercalate,
    intercalateNonNull,
    uniline,
    newline,
  )
where

import Coalmine.BaseExtras.List qualified as List
import Coalmine.Building
import Coalmine.InternalPrelude hiding (intercalate, null)
import Coalmine.LawfulConversionsInstances ()
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextLazy
import Data.Text.Lazy.Builder qualified as TextLazyBuilder
import TextBuilderDev qualified as Tb

-- * --

type TextBlock = Builder

type Splice = Builder

type MultilineTextBuilder = Builder

data Builder
  = Builder Bool (Tb.TextBuilder -> Tb.TextBuilder)

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

instance IsomorphicToTextBuilder Builder where
  toTextBuilder (Builder _ builder) =
    builder mempty
  fromTextBuilder =
    text . fromTextBuilder

instance Eq Builder where
  Builder _ l == Builder _ r =
    on (==) (to @Text) (l mempty) (r mempty)

--

instance IsSome String Builder where
  to = to . to @TextBuilder
  maybeFrom = fmap (from @Text) . maybeFrom

instance IsMany String Builder where
  from = from @Text . from

--

instance IsSome Text Builder where
  to = to . to @TextBuilder

instance IsSome Builder Text where
  to = text

instance IsMany Text Builder

instance IsMany Builder Text

instance Is Text Builder

instance Is Builder Text

--

instance IsSome TextBuilder Builder where
  to (Builder _ builder) = builder mempty

instance IsSome Builder TextBuilder where
  to = to . to @Text

instance IsMany TextBuilder Builder

instance IsMany Builder TextBuilder

instance Is TextBuilder Builder

instance Is Builder TextBuilder

--

instance IsSome TextLazy.Text Builder where
  to = to . to @TextBuilder

instance IsSome Builder TextLazy.Text where
  to = to . to @TextBuilder

instance IsMany TextLazy.Text Builder

instance IsMany Builder TextLazy.Text

instance Is TextLazy.Text Builder

instance Is Builder TextLazy.Text

--

instance IsSome TextLazyBuilder.Builder Builder where
  to = to . to @TextBuilder

instance IsSome Builder TextLazyBuilder.Builder where
  to = to . to @TextBuilder

instance IsMany TextLazyBuilder.Builder Builder

instance IsMany Builder TextLazyBuilder.Builder

instance Is TextLazyBuilder.Builder Builder

instance Is Builder TextLazyBuilder.Builder

-- * Execution

null :: Builder -> Bool
null (Builder a _) =
  a

-- * Transformation

-- | Apply a transformation only if the input is not empty.
ifNotNull :: (Builder -> Builder) -> Builder -> Builder
ifNotNull tx builder =
  if null builder
    then builder
    else tx builder

mapBuilder ::
  ( (Tb.TextBuilder -> Tb.TextBuilder) ->
    (Tb.TextBuilder -> Tb.TextBuilder)
  ) ->
  Builder ->
  Builder
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
  prefixEachLine
    . to
    $ Text.replicate amount (Text.singleton ' ')

prefixEachLine ::
  -- | Line prefix.
  -- It is your responsibility to ensure that it doesn't contain line breaks.
  Tb.TextBuilder ->
  Builder ->
  Builder
prefixEachLine prefix =
  mapBuilder $ \cont outerPrefix ->
    cont $ outerPrefix <> prefix

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

intercalateNonNull :: Builder -> [Builder] -> Builder
intercalateNonNull (Builder _sepNull _sepRender) _builders =
  Builder _null _render
  where
    _null = _sepNull && all null _builders
    _render _indent =
      Tb.intercalate
        (_sepRender _indent)
        (fmap (\(Builder _ _render) -> _render _indent) (filter (not . null) _builders))

-- * Construction

-- |
-- Extract lines from input text and indent each one of them according to the rendering settings.
text :: Text -> Builder
text text =
  Builder (Text.null text) impl
  where
    impl prefix =
      List.foldMapHeadAndTail Tb.text (foldMap lineTextMapper) lines
      where
        lines =
          Text.split (== '\n') text
        lineTextMapper lineText =
          Tb.char '\n' <> prefix <> Tb.text lineText

-- |
-- Efficiently lift a text builder, interpreting its contents as of a single line.
-- So if it contains newline characters, the contained lines won't be indented.
uniline :: Tb.TextBuilder -> Builder
uniline builder =
  Builder (Tb.null builder) $ \_ -> builder

-- |
-- Efficiently constructed newline character.
newline :: Builder
newline =
  Builder False impl
  where
    impl prefix =
      Tb.char '\n' <> prefix
