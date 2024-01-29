module Yashin.Operations.GetItem.Parser where

import Amazonka.DynamoDB qualified as Azk
import Amazonka.DynamoDB.GetItem qualified as Azk
import Coalmine.Prelude hiding (String)
import Yashin.Parsers.Attributes qualified as Attributes

newtype Response a = Response
  { run :: Azk.GetItemResponse -> Either [Error] a
  }
  deriving
    (Functor, Applicative, Alternative)
    via (ReaderT Azk.GetItemResponse (Except [Error]))

data Error
  = AttributesError [Attributes.AttributesError]
  | AttributesNotFoundError
  | UnexpectedStatusError UnexpectedStatus

data UnexpectedStatus = UnexpectedStatus
  { actual :: Int,
    expected :: Int
  }

okStatus :: Response ()
okStatus =
  Response \response ->
    if response.httpStatus < 400
      then Right ()
      else Left [UnexpectedStatusError UnexpectedStatus {actual = response.httpStatus, expected = 200}]

resourceNotFoundStatus :: Response ()
resourceNotFoundStatus =
  Response \response ->
    if response.httpStatus >= 400 && response.httpStatus < 500
      then Right ()
      else Left [UnexpectedStatusError UnexpectedStatus {actual = response.httpStatus, expected = 400}]

attributes :: Attributes.Attributes a -> Response a
attributes parser =
  Response
    \response ->
      case response.item of
        Just item -> case parser.parser item of
          Right res -> Right res
          Left err -> Left [AttributesError err]
        Nothing -> Left [AttributesNotFoundError]
