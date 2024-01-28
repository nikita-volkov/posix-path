module Yashin.Operation where

import Amazonka qualified as Azk
import Coalmine.Prelude hiding (String)
import Yashin.Session qualified as Session

newtype Operation i o = Operation
  { run :: i -> Session.Session o
  }

exchange :: (Azk.AWSRequest request, Typeable request, Typeable (Azk.AWSResponse request)) => Operation request (Azk.AWSResponse request)
exchange = Operation Session.exchange
