module Coalmine.Fx.FileSys where

import Prelude
import Fx
import qualified System.IO.Temp as Temporary
import qualified System.Directory as Directory
import qualified Data.Text.IO as Text


-- * Providers
-------------------------

provideTmpDir :: Provider IOError FilePath
provideTmpDir = acquireAndRelease createTmpDir deleteDir


-- * Effects
-------------------------

{-|
Wrapper around @`Directory.createDirectoryIfMissing` `True`@.
-}
createDirectoryRecursively :: FilePath -> Fx env IOError ()
createDirectoryRecursively path = runExceptionalIO (Directory.createDirectoryIfMissing True path)

{-|
Wrapper around `Directory.listDirectory`.
-}
listDirectory :: FilePath -> Fx env IOError [FilePath]
listDirectory path = runExceptionalIO (Directory.listDirectory path)

{-|
Wrapper around `Directory.removeFile`.
-}
removeFile :: FilePath -> Fx env IOError ()
removeFile path = runExceptionalIO (Directory.removeFile path)

{-|
Create a temporary directory.
-}
createTmpDir :: Fx env IOError FilePath
createTmpDir = runExceptionalIO $ do
  dir <- Temporary.getCanonicalTemporaryDirectory
  Temporary.createTempDirectory dir "coalmine"

{-|
Delete directory.
-}
deleteDir :: FilePath -> Fx env IOError ()
deleteDir dir = runExceptionalIO $ Directory.removeDirectoryRecursive dir

{-|
Move file to one location from another, producing its new file path.
-}
moveFile :: FilePath -> FilePath -> Fx env IOError FilePath
moveFile to from = error "TODO"

writeTextToFile :: FilePath -> Text -> Fx env IOError ()
writeTextToFile path text = runExceptionalIO $ Text.writeFile path text
