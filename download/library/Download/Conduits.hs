-- | Helper adaptations of conduit.
module Download.Conduits where

import Conduit
import Data.Conduit.Binary qualified as ConduitExtra.Binary
import Data.Conduit.Lzma qualified as LzmaConduit
import Data.Conduit.Tar qualified as TarConduit
import Data.Text.Encoding qualified as Text
import PosixPath (Path)
import PosixPath qualified as Path
import Prelude

untarSink :: (MonadResource m, MonadThrow m) => Path -> ConduitT ByteString Void m ()
untarSink basePath =
  TarConduit.untar fileInfoSink
  where
    fileInfoSink fileInfo = do
      entryPath <- liftIO $ case Text.decodeUtf8' (TarConduit.filePath fileInfo) of
        Left _ -> fail ("Invalid UTF8: " <> show (TarConduit.filePath fileInfo))
        Right text -> case Path.parseText text of
          Nothing -> fail ("Invalid path: " <> show text)
          Just path -> return path
      ConduitExtra.Binary.sinkFile (Path.toFilePath (basePath <> entryPath))

fileSink :: (MonadResource m) => Path -> ConduitT ByteString Void m ()
fileSink path =
  ConduitExtra.Binary.sinkFile (Path.toFilePath path)

unxzPipe :: (MonadThrow m, MonadIO m) => ConduitM ByteString ByteString m ()
unxzPipe =
  LzmaConduit.decompress Nothing
