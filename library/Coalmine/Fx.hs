module Coalmine.Fx where

import Prelude
import Fx
import qualified Turtle


runFxFailing :: (err -> String) -> Fx () err a -> IO a
runFxFailing show fx = do
  either <- runFx (exposeErr fx)
  case either of
    Right a -> return a
    Left err -> fail (show err)

{-|
Run a cmd, failing with its stderr output in case of non-zero return code.
-}
runCmd :: Text -> Fx env (Text, Text) Text
runCmd cmd = do
  (exitCode, out, err) <- runTotalIO (Turtle.shellStrictWithErr cmd empty)
  case exitCode of
    Turtle.ExitSuccess -> return out
    Turtle.ExitFailure _ -> throwErr (err, out)

compressFile :: FilePath -> Fx env (Text, Text) FilePath
compressFile path = do
  runCmd ("xz -zfq7e " <> fromString path)
  return (path <> ".xz")
