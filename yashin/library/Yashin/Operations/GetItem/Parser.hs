module Yashin.Operations.GetItem.Parser where

import Amazonka.DynamoDB qualified as Azk
import Amazonka.DynamoDB.GetItem qualified as Azk
import Coalmine.Prelude hiding (String)
import Yashin.Parsers.Attributes qualified as Attributes

data Response a = Response
  { run :: Azk.GetItemResponse -> Either Error a
  }

data Error
  = AttributesError [Attributes.AttributesError]
  | NotFoundError

attributes :: Attributes.Attributes a -> Response a
attributes parser =
  Response
    \response ->
      case response.item of
        Just item -> case parser.parser item of
          Right res -> Right res
          Left err -> Left (AttributesError err)
        Nothing -> Left NotFoundError
