module Download
  ( downloadToFile,
  )
where

import Data.ByteString qualified as ByteString
import Network.HTTP.Client qualified as Hc
import PosixPath (Path)
import PosixPath qualified as Path
import System.IO qualified
import Prelude

downloadToFile ::
  -- | URL.
  Text ->
  -- | Local path.
  Path ->
  IO ()
downloadToFile url localPath = do
  request <- Hc.parseRequest $ toList url
  manager <- Hc.newManager Hc.defaultManagerSettings
  Hc.withResponse request manager (consumeBodyReaderToFile localPath . Hc.responseBody)

consumeBodyReaderToFile ::
  Path ->
  Hc.BodyReader ->
  IO ()
consumeBodyReaderToFile localPath read = do
  handle <- System.IO.openFile (Path.toFilePath localPath) System.IO.WriteMode
  let stream = do
        chunk <- read
        if ByteString.null chunk
          then return ()
          else do
            ByteString.hPut handle chunk
            stream
  finally stream (System.IO.hClose handle)
