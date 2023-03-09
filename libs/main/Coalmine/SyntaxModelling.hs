module Coalmine.SyntaxModelling where

import Coalmine.InternalPrelude hiding (toTextBuilder)
import Data.Attoparsec.Text qualified as Attoparsec

-- |
-- Canonical syntactic representation for a data-type.
--
-- The data-types are supposed to be provided via newtype-wrappers
-- to provide various syntaxes for data-types.
--
-- Provides means to parse and render a value in such a way
-- that parsing a rendered value will be successful and
-- will produce the same value.
-- That is __the law__ of this typeclass.
class Syntax a where
  attoparsecParser :: Attoparsec.Parser a
  toTextBuilder :: a -> TextBuilder

completeAttoparsecParser :: (Syntax a) => Attoparsec.Parser a
completeAttoparsecParser =
  attoparsecParser <* Attoparsec.endOfInput

-- | Helper for defining the 'IsString' instances.
fromStringUnsafe :: (HasCallStack, Syntax a) => String -> a
fromStringUnsafe =
  either error id
    . Attoparsec.parseOnly completeAttoparsecParser
    . fromString

-- | Parse text.
fromTextInEither :: (Syntax a) => Text -> Either Text a
fromTextInEither =
  first fromString
    . Attoparsec.parseOnly completeAttoparsecParser

-- | Parse text and simplify the errors to 'Nothing'.
fromTextInMaybe :: (Syntax a) => Text -> Maybe a
fromTextInMaybe =
  either (const Nothing) Just
    . Attoparsec.parseOnly completeAttoparsecParser

-- | Render to text.
toText :: (Syntax a) => a -> Text
toText =
  to @Text . toTextBuilder
