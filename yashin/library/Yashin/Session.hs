module Yashin.Session where

import Amazonka qualified as Azk
import Coalmine.Prelude hiding (String)
import Conduit (ResourceT)

newtype Session a
  = Session (Azk.Env -> ResourceT IO (Either Azk.Error a))
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT Azk.Env (ExceptT Azk.Error (ResourceT IO)))

exchange :: (Azk.AWSRequest request, Typeable request, Typeable (Azk.AWSResponse request)) => request -> Session (Azk.AWSResponse request)
exchange request =
  Session \env ->
    Azk.sendEither env request
