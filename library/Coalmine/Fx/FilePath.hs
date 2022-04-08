module Coalmine.Fx.FilePath where

import Coalmine.InternalPrelude
import qualified Data.Text.IO as Text
import Fx
import qualified System.Directory as Directory
import qualified System.IO.Temp as Temporary

-- * Providers

provideTmpDir :: Provider IOError FilePath
provideTmpDir = acquireAndRelease createTmpDir deleteDir

-- * Effects

-- |
-- Wrapper around @`Directory.createDirectoryIfMissing` `True`@.
createDirectoryRecursively :: Fx FilePath IOError ()
createDirectoryRecursively = runExceptionalIO (Directory.createDirectoryIfMissing True)

-- |
-- Wrapper around `Directory.listDirectory`.
listDirectory :: Fx FilePath IOError [FilePath]
listDirectory = runExceptionalIO Directory.listDirectory

-- |
-- Wrapper around `Directory.removeFile`.
removeFile :: Fx FilePath IOError ()
removeFile = runExceptionalIO Directory.removeFile

-- |
-- Create a temporary directory.
createTmpDir :: Fx env IOError FilePath
createTmpDir = runExceptionalIO $
  const $ do
    dir <- Temporary.getCanonicalTemporaryDirectory
    Temporary.createTempDirectory dir "coalmine"

-- |
-- Delete directory.
deleteDir :: Fx FilePath IOError ()
deleteDir = runExceptionalIO Directory.removeDirectoryRecursive

-- |
-- Move file to one location from another.
moveFile :: FilePath -> Fx FilePath IOError ()
moveFile to = error "TODO"

writeText :: Text -> Fx FilePath IOError ()
writeText text = runExceptionalIO $ \path -> Text.writeFile path text

setAsCurrentDirectory :: Fx FilePath IOError ()
setAsCurrentDirectory = runExceptionalIO Directory.setCurrentDirectory
