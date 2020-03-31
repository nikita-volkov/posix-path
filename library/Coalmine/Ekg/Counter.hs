module Coalmine.Ekg.Counter
where

import Coalmine.Prelude
import Fx
import System.Remote.Counter (Counter)
import System.Remote.Monitoring (Server)
import qualified System.Remote.Monitoring as Server
import qualified System.Remote.Counter as Counter


provider :: Server -> Text -> Provider err Counter
provider server name =
  runFx $ runTotalIO $ const $ Server.getCounter name server

add :: Int64 -> Fx Counter err ()
add value = runTotalIO $ \ env -> Counter.add env value

inc :: Fx Counter err ()
inc = runTotalIO $ Counter.inc
