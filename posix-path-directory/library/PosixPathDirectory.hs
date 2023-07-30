module PosixPathDirectory
  ( listDirectory,
  )
where

import Coalmine.Prelude hiding (Path)
import PosixPath
import System.Directory qualified as Directory

-- | Adaptation of "Directory.listDirectory".
listDirectory :: Path -> IO [Path]
listDirectory path =
  Directory.listDirectory (toFilePath path)
    >>= traverse parseFilePath

-- * Helpers

parseFilePath :: FilePath -> IO Path
parseFilePath path =
  fromFilePath path
    & maybe (fail ("Invalid path: " <> path)) return
