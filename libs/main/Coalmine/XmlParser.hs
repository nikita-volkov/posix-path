-- |
-- High-performance streaming single-pass exclusively
-- syntactic parser with a high-level API.
--
-- The performance is achieved by sacrificing the alternative instance.
-- It still takes on most of the heavy lifting of parsing efficiently
-- and provides a simple and highly flexible API.
module Coalmine.XmlParser where

import Coalmine.InternalPrelude
import Coalmine.XmlParser.Attoparsec qualified as A
import Data.Attoparsec.Text qualified as A

data ParseNodes a

-- |
-- Parsing an element means first parsing its tag name,
-- then parsing its attributes,
-- then parsing its children.
-- That\'s exactly the actions we make the user take
-- and in that only order.
element :: ParseName (ParseAttributes (ParseNodes a)) -> ParseNodes a
element =
  error "TODO"

text :: ParseContent a -> ParseNodes a
text =
  error "TODO"

-- | Name parser.
-- Applies to both tag-names and attribute names.
newtype ParseName a
  = ParseName (A.Parser a)

refinedName :: (Maybe Text -> Text -> Either Text a) -> ParseName a
refinedName k =
  ParseName $ do
    res <- A.name k
    case res of
      Left err -> fail (toList err)
      Right res -> return res

anyName :: (Maybe Text -> Text -> a) -> ParseName a
anyName k =
  ParseName $ A.name k

data ParseAttributes a

attribute :: ParseName (ParseContent a) -> ParseAttributes a
attribute =
  error "TODO"

data ParseContent a

freeFormContent :: ParseContent Text
freeFormContent =
  error "TODO"

qNameContent :: ParseName a -> ParseContent a
qNameContent =
  error "TODO"
