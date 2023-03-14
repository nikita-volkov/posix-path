module Coalmine.FileSystem
  ( atPathAsWorkDir,
  )
where

import Coalmine.EvenSimplerPaths (Path)
import Coalmine.InternalPrelude hiding (print)
import Coalmine.Printing
import Turtle qualified

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
