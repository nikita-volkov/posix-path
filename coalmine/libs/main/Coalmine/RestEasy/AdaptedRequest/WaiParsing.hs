-- |
-- Interpreter of WAI request.
module Coalmine.RestEasy.AdaptedRequest.WaiParsing where

import Coalmine.InternalPrelude
import Coalmine.RestEasy.AdaptedRequest.Model
import Coalmine.RestEasy.MimeTypeLists qualified as MimeTypeLists
import Network.HTTP.Media qualified as HttpMedia

contentType :: ByteString -> Maybe Type
contentType = HttpMedia.mapContentMedia mimeTypeListAssocs

accept :: ByteString -> Maybe Type
accept = HttpMedia.mapAcceptMedia mimeTypeListAssocs

mimeTypeListAssocs :: [(HttpMedia.MediaType, Type)]
mimeTypeListAssocs =
  mconcat
    [ assoc TextType MimeTypeLists.text,
      assoc HtmlType MimeTypeLists.html,
      assoc XmlType MimeTypeLists.xml,
      assoc JsonType MimeTypeLists.json,
      assoc YamlType MimeTypeLists.yaml,
      assoc BinaryType MimeTypeLists.binary
    ]
  where
    assoc a b = fmap (,a) b
