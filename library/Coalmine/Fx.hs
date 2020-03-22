module Coalmine.Fx where

import Prelude
import Fx
import qualified Turtle


runFxFailing :: Show err => Fx () err a -> IO a
runFxFailing = runFx . handleErr (fail . show)

runCmd :: Text -> Fx env Int ()
runCmd cmd = do
  exitCode <- runTotalIO (Turtle.shell cmd empty)
  case exitCode of
    Turtle.ExitSuccess -> return ()
    Turtle.ExitFailure code -> throwErr code

compressFile :: FilePath -> Fx env Int FilePath
compressFile path = do
  runCmd ("xz -zfq7e " <> fromString path)
  return (path <> ".xz")
