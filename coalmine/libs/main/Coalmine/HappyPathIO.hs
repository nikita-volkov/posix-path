-- |
-- IO that implies that its failing should short-circuit the application.
--
-- IOW, the actions here are not meant to be caught exceptions from.
--
-- This perspective lets us completely avoid exceptions.
--
-- This is achieved by delegating it to the action to terminate the application.
-- E.g., outputting something before exiting and etc.
module Coalmine.HappyPathIO where

import Coalmine.BaseExtras.List qualified as List
import Coalmine.EvenSimplerPaths (Path)
import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.Inter
import Coalmine.InternalPrelude
import Coalmine.Parsing
import Coalmine.Printing
import Data.Attoparsec.Text qualified as Attoparsec
import Data.ByteString qualified as ByteString
import Data.Text.IO qualified as TextIO

-- * --

-- | Read one of files.
readOneOf :: [Path] -> IO ByteString
readOneOf = go []
  where
    go !errs = \case
      path : tail ->
        catch @SomeException
          (ByteString.readFile (printCompactAsString path))
          (\e -> go ((e, path) : errs) tail)
      [] ->
        die (from @TextBuilder report)
        where
          report =
            "Failed to read from any of the following files:\n"
              <> list
            where
              list = List.mapIntercalate errReport "\n" errs
              errReport (_err, path) =
                "- " <> printCompactAs path

-- | Load and parse a required environment variable.
loadRequiredEnv :: (LenientParser a) => Text -> IO a
loadRequiredEnv name = do
  loadNonRequiredEnv name >>= \case
    Nothing -> die [i|Env var $name not found|]
    Just res -> return res

-- | Load and parse a non-required environment variable.
loadNonRequiredEnv :: (LenientParser a) => Text -> IO (Maybe a)
loadNonRequiredEnv name = do
  env <- fmap to <$> lookupEnv (to name)
  case env of
    Just env -> case parse parser env of
      Left err ->
        die
          [i|
            Failed to parse env var $name.
            Reason:
              $err
            Input:
              $env
          |]
      Right res -> return $ Just res
    Nothing -> return Nothing
  where
    parser =
      lenientParser <* Attoparsec.skipSpace

findOneFileByExtensions :: [Text] -> IO Path
findOneFileByExtensions extensions = do
  files <- filter ((==) extensions . Path.extensions) <$> Path.listDirectory "."
  case files of
    [file] -> return file
    [] -> die $ "No file found with following extensions: " <> show extensions
    _ -> die $ "More than one file found: " <> show files

editFile :: Path -> (Text -> IO Text) -> IO ()
editFile path onText =
  TextIO.readFile pathString >>= onText >>= TextIO.writeFile pathString
  where
    pathString = Path.toString path
