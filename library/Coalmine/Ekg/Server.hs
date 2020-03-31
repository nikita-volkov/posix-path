module Coalmine.Ekg.Server
where

import Coalmine.Prelude
import Fx
import System.Remote.Monitoring (Server)
import qualified System.Remote.Monitoring as Server
import qualified Data.Text.Encoding as Text


provider :: Text -> Word16 -> Provider err Server
provider host port =
  acquireAndRelease
    (runTotalIO $ const $ Server.forkServer (Text.encodeUtf8 host) (fromIntegral port))
    (runTotalIO $ \ server -> killThread (Server.serverThreadId server))

customProvider :: Text -> Word16 -> (Server -> Provider err env) -> Provider err env
customProvider host port subProvider = provider host port >>= subProvider
