module Coalmine.Fx where

import Prelude
import Fx
import qualified Turtle


runFxFailing :: Show err => Fx () err a -> IO a
runFxFailing = runFx . handleErr (fail . show)

{-|
Run a cmd, failing with its stderr output in case of non-zero return code.
-}
runCmd :: Text -> Fx env Text Text
runCmd cmd = do
  (exitCode, out, err) <- runTotalIO (Turtle.shellStrictWithErr cmd empty)
  case exitCode of
    Turtle.ExitSuccess -> return out
    Turtle.ExitFailure _ -> throwErr err

compressFile :: FilePath -> Fx env Text FilePath
compressFile path = do
  runCmd ("xz -zfq7e " <> fromString path)
  return (path <> ".xz")
