-- TODO: Consider renaming: 'literalParser' to 'parseLiteral' and 'literalTextBuilder' to 'printLiteral'.

-- |
-- Common interface for custom data-types which can be represented with a custom textual literal.
--
-- Provides a reusable compile-time constructor, parsing and rendering APIs.
module Coalmine.Literal where

import Coalmine.InternalPrelude
import Coalmine.Parsing
import Data.Attoparsec.Text qualified as Attoparsec
import Language.Haskell.TH.Syntax qualified as TH
import TextBuilderDev qualified

-- | Value that has a textual representation.
--
-- This class is lawful: rendering the value and then parsing it
-- should succeed and produce the original value.
--
-- The law can be represented with the following code:
-- @
-- Right a
--   == 'Attoparsec.parseOnly'
--     ('literalParser' <* 'Attoparsec.endOfInput')
--     ('to' ('literalCompactTextBuilder' a))
-- @
class Literal a where
  literalParser :: Attoparsec.Parser a
  literalTextBuilder :: a -> TextBuilderDev.TextBuilder

-- | If a literal can also be converted to code, we can instantiate and
-- validate it at compile time, thus letting us provide guarantees that a
-- value constructed this way is correct.
--
-- Examples:
--
-- > examplePath :: Path
-- > examplePath = $$(l "/usr/local/bin")
--
-- > exampleOrg :: Url
-- > exampleOrg = $$(l "http://example.org")
--
-- > userAtExampleOrg :: Email
-- > userAtExampleOrg = $$(l "user@example.org")
--
-- > phoneNumber :: PhoneNumber
-- > phoneNumber = $$(l "+79160123456")
--
-- > samePhoneNumber :: PhoneNumber
-- > samePhoneNumber = $$(l "+7(916)012-3456")
--
-- > exampleUuid :: Uuid
-- > exampleUuid = $$(l "123e4567-e89b-12d3-a456-426614174000")
--
-- > -- | Construct text not by converting from a string literal,
-- > -- but by packing a byte-array literal, which could be more efficient.
-- > exampleText :: Text
-- > exampleText = $$(l "Example text")
l :: (Literal a, Lift a) => String -> TH.Q (TH.TExp a)
l literal = do
  literal <- case parseWith literalParser (to literal) of
    Right literal -> return literal
    Left err -> fail $ to err
  TH.examineCode $ TH.liftTyped literal

toText :: (Literal a) => a -> Text
toText = to . literalTextBuilder

parseText :: (Literal a) => Text -> Either Text a
parseText =
  first fromString
    . Attoparsec.parseOnly (literalParser <* Attoparsec.endOfInput)
