module Coalmine.CliKit.Clio where

import Coalmine.CliKit.Clio.TerminalMarkup qualified as TerminalMarkup
import Coalmine.EvenSimplerPaths (Path)
import Coalmine.EvenSimplerPaths qualified as Path
import Coalmine.InternalPrelude hiding (readFile, writeFile)
import Coalmine.TerminalMarkup (TerminalMarkup)
import Coalmine.TerminalMarkup qualified as TerminalMarkup
import Data.Text.IO qualified as TextIO
import Options.Applicative qualified as OptparseApplicative

data OutputFormatting
  = -- | Print plain text.
    PlainOutputFormatting
  | -- | Use the ANSI commands to prettify the output.
    PrettyOutputFormatting
  | -- | Automatically detect the terminal type and choose Pretty
    -- formatting if possible.
    AutoOutputFormatting

-- | Use that to define the main function.
clioApp ::
  -- | Formatting of all output of this app.
  OutputFormatting ->
  -- | Description of the application.
  Text ->
  -- | Parse options and produce a Clio app.
  OptparseApplicative.Parser (Clio ()) ->
  IO ()
clioApp =
  error "TODO"

newtype Clio a = Clio (IO a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadIO Clio where
  -- TODO: Reimplement by rendering the exceptions in a prettier fashion.
  -- E.g., we definitely can cover the standard exceptions like IOError.
  liftIO = Clio . liftIO

-- |
-- Fail with the specified message.
terminate :: TerminalMarkup -> Clio a
terminate =
  error "TODO"

listDirectory :: Path -> Clio [Path]
listDirectory =
  liftIO . Path.listDirectory

readFile :: Path -> Clio Text
readFile =
  liftIO . TextIO.readFile . Path.toString

writeFile :: Path -> Text -> Clio ()
writeFile path text =
  liftIO $ TextIO.writeFile (Path.toString path) text

editFile :: Path -> (Text -> Clio Text) -> Clio ()
editFile path onText =
  readFile path >>= onText >>= writeFile path

-- |
-- Use the Alternative instance to make it optional.
findOneFileByExtensions :: [Text] -> Clio Path
findOneFileByExtensions extensions = do
  files <- filter ((==) extensions . Path.extensions) <$> listDirectory "."
  case files of
    [file] -> return file
    [] ->
      terminate $
        "No file found with the following extensions: \"" <> TerminalMarkup.extensions extensions <> "\""
    _ ->
      terminate $
        "More than one file found:\n"
          <> TerminalMarkup.multilineListing (fmap TerminalMarkup.path files)
