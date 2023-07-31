module PosixPathDirectory
  ( listDirectory,
  )
where

import Coalmine.Prelude hiding (Path)
import PosixPath (Path)
import PosixPath qualified as Path
import System.Directory qualified as Directory

-- | Adaptation of "Directory.listDirectory".
listDirectory :: Path -> IO [Path]
listDirectory path =
  Directory.listDirectory (Path.toFilePath path)
    >>= traverse parseFilePath

-- * Helpers

parseFilePath :: FilePath -> IO Path
parseFilePath path =
  Path.parseFilePath path
    & maybe (fail ("Invalid path: " <> path)) return
