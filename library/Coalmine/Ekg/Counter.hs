module Coalmine.Ekg.Counter where

import Coalmine.Prelude
import Fx
import System.Remote.Counter (Counter)
import qualified System.Remote.Counter as Counter
import System.Remote.Monitoring (Server)
import qualified System.Remote.Monitoring as Server

type Env = Counter

provider :: Server -> Text -> Provider err Env
provider server name =
  runFx $ runTotalIO $ const $ Server.getCounter name server

add :: Int64 -> Fx Env err ()
add value = runTotalIO $ \env -> Counter.add env value

inc :: Fx Env err ()
inc = runTotalIO $ Counter.inc
