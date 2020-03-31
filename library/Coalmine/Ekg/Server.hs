module Coalmine.Ekg.Server
where

import Coalmine.Prelude
import Fx
import System.Remote.Monitoring (Server)
import qualified System.Remote.Monitoring as Server
import qualified Data.Text.Encoding as Text


type Env = Server

provider :: Text -> Word16 -> Provider err Env
provider host port =
  acquireAndRelease
    (runTotalIO $ const $ Server.forkServer (Text.encodeUtf8 host) (fromIntegral port))
    (runTotalIO $ \ server -> killThread (Server.serverThreadId server))

extendedProvider :: Text -> Word16 -> (Server -> Provider err env) -> Provider err env
extendedProvider host port subProvider = provider host port >>= subProvider
