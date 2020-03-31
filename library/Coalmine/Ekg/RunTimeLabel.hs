module Coalmine.Ekg.RunTimeLabel
where

import Coalmine.Prelude
import Fx
import System.Remote.Monitoring (Server)
import System.Remote.Label (Label)
import qualified System.Remote.Monitoring as Server
import qualified System.Remote.Label as Label
import qualified Text.Builder


provider :: Server -> Text -> Provider err ()
provider server name =
  void $
  acquireAndRelease
    (do
      aliveFlag <- runFx $ runTotalIO $ const $ newIORef True
      runFx $ runTotalIO $ const $ do
        label <- Server.getLabel name server
        forkIO $ do
          startTime <- getCurrentTime
          fix $ \ loop -> do
            alive <- readIORef aliveFlag
            when alive $ do
              currentTime <- getCurrentTime
              let
                interval = diffUTCTime currentTime startTime
                rendering = fromBuilder (Text.Builder.intervalInSeconds interval)
                in Label.set label rendering
              threadDelay 100_000
              loop
      return aliveFlag)
    (runTotalIO $ \ aliveFlag -> writeIORef aliveFlag False)
