module Yashin.Operations.GetItem.Parser where

import Amazonka.DynamoDB qualified as Azk
import Amazonka.DynamoDB.GetItem qualified as Azk
import Coalmine.Prelude hiding (String)
import Yashin.Parsers.Attributes qualified as Attributes

newtype Response a = Response
  { run :: Azk.GetItemResponse -> Either [Error] a
  }
  deriving
    (Functor, Applicative, Alternative, Monad, MonadPlus)
    via (ReaderT Azk.GetItemResponse (Except [Error]))

data Error
  = AttributesError [Attributes.AttributesError]
  | AttributesNotFoundError

attributes :: Attributes.Attributes a -> Response a
attributes parser =
  Response
    \response ->
      case response.item of
        Just item -> case parser.parser item of
          Right res -> Right res
          Left err -> Left [AttributesError err]
        Nothing -> Left [AttributesNotFoundError]
