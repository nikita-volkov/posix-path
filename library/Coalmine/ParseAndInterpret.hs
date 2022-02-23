module Coalmine.ParseAndInterpret
  ( -- *
    parseAndInterpretDoc,
    parseAndInterpretDocFile,
    interpretDoc,

    -- *
    Interpret,
    focusOn,
    fail,
  )
where

import Coalmine.InternalPrelude hiding (fail)
import Coalmine.Located (Located)
import qualified Coalmine.Located as Located
import qualified Coalmine.SimplePaths as Paths
import qualified Data.Text.IO as TextIO

-- *

parseAndInterpretDocFile ::
  -- | Parse text to AST.
  (Text -> Either Text ast) ->
  -- | Interpret the AST.
  (ast -> Interpret state ()) ->
  -- | Initial state.
  state ->
  -- | Input file.
  Paths.FilePath ->
  -- | Fail or produce an updated state.
  IO (Either Text state)
parseAndInterpretDocFile parse interpret state filePath = do
  TextIO.readFile (toString filePath)
    <&> parseAndInterpretDoc parse interpret state

parseAndInterpretDoc ::
  -- | Parse text to AST.
  (Text -> Either Text ast) ->
  -- | Interpret the AST.
  (ast -> Interpret state ()) ->
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
interpretDoc :: Interpret state () -> state -> Text -> Either Text state
interpretDoc (Interpret update) state input =
  case update (0, 0, state) of
    Left err -> Left $ Located.renderInMegaparsecStyle err input
    Right ((), (_, _, state)) -> Right state

-- *

newtype Interpret state res
  = Interpret
      ((Int, Int, state) -> Either (Located Text) (res, (Int, Int, state)))
  deriving
    (Functor, Applicative, Monad)
    via (StateT (Int, Int, state) (Either (Located Text)))

instance MonadState state (Interpret state) where
  state f = Interpret $ \(a, b, state) -> Right $ case f state of
    (res, state) -> (res, (a, b, state))

-- *

-- |
-- Focus on the provided value associated with location.
--
-- All following calls to fail will be associated with that location,
-- until you call 'focusOn' again.
focusOn :: Located a -> Interpret state a
focusOn (Located.Located start end val) =
  Interpret $ \(_, _, state) -> Right (val, (start, end, state))

fail :: Text -> Interpret state a
fail msg =
  Interpret $ \(start, end, _) -> Left (Located.Located start end msg)
