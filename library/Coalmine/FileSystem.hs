module Coalmine.FileSystem
  ( atPathAsWorkDir,
  )
where

import Coalmine.EvenSimplerPaths (Path)
import qualified Coalmine.EvenSimplerPaths as Path
import Coalmine.InternalPrelude hiding (print)
import Coalmine.Printing
import qualified Data.Text.IO as TextIO
import qualified Turtle

atPathAsWorkDir :: Path -> IO a -> IO a
atPathAsWorkDir path io =
  bracket acquire release use
  where
    acquire = do
      takeMVar globalCdLock
      previousWorkDir <- Turtle.pwd
      Turtle.cd (printCompactAs @String path)
      return previousWorkDir
    release previousWorkDir = do
      Turtle.cd previousWorkDir
      putMVar globalCdLock ()
    use _ = io

{-# NOINLINE globalCdLock #-}
globalCdLock :: MVar ()
globalCdLock =
  unsafePerformIO $ newMVar ()
