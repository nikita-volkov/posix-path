-- | High-performance streaming single-pass parser
-- with a high-level API.
-- That is achieved by sacrificing the alternative instance,
-- which however still takes on most of the heavy lifting
-- of parsing in a single pass with a typesafe,
-- yet simple and highly flexible API.
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

parsedName :: (Maybe Text -> Text -> Either Text a) -> ParseName a
parsedName k =
  ParseName $ join $ A.name $ \a b ->
    case ( case b of
             Nothing -> k Nothing a
             Just b -> k (Just a) b
         ) of
      Left err -> fail (toList err)
      Right res -> return res

anyName :: ParseName ()
anyName =
  ParseName $ A.name $ const $ const ()

data ParseAttributes a

attribute :: ParseName (ParseContent a) -> ParseAttributes a
attribute =
  error "TODO"

data ParseContent a
