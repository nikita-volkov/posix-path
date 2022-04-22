module Coalmine.Interpreter
  ( -- *
    parseAndInterpretDoc,
    parseAndInterpretDocFile,
    interpretDoc,

    -- *
    Update,
    focusOn,
    fail,
    getsFailing,
  )
where

import Coalmine.InternalPrelude hiding (fail)
import Coalmine.Located (Located)
import qualified Coalmine.Located as Located
import Coalmine.Printing
import qualified Coalmine.SimplePaths as Paths
import Coalmine.StringIsomorphism
import Coalmine.TextIsomorphism
import qualified Data.Text.IO as TextIO

-- *

parseAndInterpretDocFile ::
  -- | Parse text to AST.
  (Text -> Either Text ast) ->
  -- | Interpret the AST.
  (ast -> Update state ()) ->
  -- | Initial state.
  state ->
  -- | Input file.
  Paths.FilePath ->
  -- | Fail or produce an updated state.
  IO (Either Text state)
parseAndInterpretDocFile parse interpret state filePath = do
  TextIO.readFile (printCompactAsString filePath)
    <&> parseAndInterpretDoc parse interpret state

parseAndInterpretDoc ::
  -- | Parse text to AST.
  (Text -> Either Text ast) ->
  -- | Interpret the AST.
  (ast -> Update state ()) ->
  -- | Initial state.
  state ->
  -- | Input text.
  Text ->
  -- | Fail or produce an updated state.
  Either Text state
parseAndInterpretDoc parse interpret state input =
  parse input >>= \ast -> interpretDoc (interpret ast) state input

-- |
-- Interpret document as a whole.
--
-- Allows to abstract over the positions in the doc and
-- avoid manual distribution of input.
interpretDoc :: Update state () -> state -> Text -> Either Text state
interpretDoc (Update update) state input =
  case update (0, 0, state) of
    Left err -> Left $ Located.renderInMegaparsecStyle err input
    Right ((), (_, _, state)) -> Right state

-- *

newtype Update state res
  = Update
      ((Int, Int, state) -> Either (Located Text) (res, (Int, Int, state)))
  deriving
    (Functor, Applicative, Monad)
    via (StateT (Int, Int, state) (Either (Located Text)))

instance MonadState state (Update state) where
  state f = Update $ \(a, b, state) -> Right $ case f state of
    (res, state) -> (res, (a, b, state))

-- *

-- |
-- Focus on the provided value associated with location.
--
-- All following calls to fail will be associated with that location,
-- until you call 'focusOn' again.
focusOn :: Located a -> Update state a
focusOn (Located.Located start end val) =
  Update $ \(_, _, state) -> Right (val, (start, end, state))

fail :: Text -> Update state a
fail msg =
  Update $ \(start, end, _) -> Left (Located.Located start end msg)

getsFailing :: (state -> Either Text a) -> Update state a
getsFailing fn =
  Update $ \(start, end, state) ->
    bimap (Located.Located start end) (,(start, end, state)) $ fn state
