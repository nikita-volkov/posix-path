module Download
  ( download,
    downloadProcessing,
    Processing (..),
  )
where

import Conduit qualified
import Data.ByteString qualified as ByteString
import Download.Conduits qualified as Conduits
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Simple qualified as HttpConduit
import PosixPath (Path)
import PosixPath qualified as Path
import System.IO qualified
import Prelude

download ::
  -- | URL.
  Text ->
  -- | Local path.
  Path ->
  IO ()
download url localPath = do
  request <- HttpClient.parseRequest $ toList url
  manager <- HttpClient.newManager HttpClient.defaultManagerSettings
  HttpClient.withResponse request manager (consumeBodyReaderToFile localPath . HttpClient.responseBody)

consumeBodyReaderToFile ::
  Path ->
  HttpClient.BodyReader ->
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

data Processing
  = UnxzProcessing

downloadProcessing ::
  -- | Stages of intermediate processing.
  [Processing] ->
  -- | Untar.
  Bool ->
  -- | URL.
  Text ->
  -- | Local path.
  Path ->
  IO ()
downloadProcessing stages untar url localPath = do
  request <- HttpClient.parseRequest $ toList url
  Conduit.runResourceT $ HttpConduit.httpSink request $ \_ -> sink
  where
    sink =
      foldr cons nil stages
      where
        cons =
          Conduit.fuse . \case
            UnxzProcessing -> Conduits.unxzPipe
        nil =
          if untar
            then Conduits.untarSink localPath
            else Conduits.fileSink localPath
