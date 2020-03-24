module Coalmine.Fx where

import Coalmine.InternalPrelude
import Fx
import qualified Turtle


{-|
Run a cmd, failing with its stderr output in case of non-zero return code.
-}
runCmd :: Text -> Fx env (Text, Text) Text
runCmd cmd = do
  (exitCode, out, err) <- runTotalIO (const (Turtle.shellStrictWithErr cmd empty))
  case exitCode of
    Turtle.ExitSuccess -> return out
    Turtle.ExitFailure _ -> throwErr (err, out)

compressFile :: FilePath -> Fx env (Text, Text) FilePath
compressFile path = do
  runCmd ("xz -zfq7e " <> fromString path)
  return (path <> ".xz")
