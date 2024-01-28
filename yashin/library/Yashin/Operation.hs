module Yashin.Operation where

import Amazonka qualified as Azk
import Amazonka.DynamoDB.GetItem qualified as Azk
import Coalmine.Prelude hiding (String)
import Yashin.Operations.GetItem.Parser qualified as GetItem.Parser
import Yashin.Session qualified as Session

newtype Operation i o = Operation
  { run :: i -> Session.Session (Either OpError o)
  }

data OpError
  = GetItemParserOpError [GetItem.Parser.Error]

getItem :: GetItem.Parser.Response a -> Operation Azk.GetItem a
getItem parser =
  Operation
    (fmap (first GetItemParserOpError . parser.run) . Session.exchange)
