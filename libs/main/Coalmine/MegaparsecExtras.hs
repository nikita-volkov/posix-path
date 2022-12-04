module Coalmine.MegaparsecExtras where

import Coalmine.InternalPrelude hiding (try)
import Coalmine.Located qualified as Located
import Coalmine.Parsed qualified as Parsed
import Text.Megaparsec

-- |
-- Execute as text refiner.
toTextRefiner :: Parsec Void Text a -> Text -> Either Text a
toTextRefiner p = left (fromString . errorBundlePretty) . runParser (p <* eof) ""

-- * --

liftEither :: Stream s => Either Text a -> Parsec e s a
liftEither = \case
  Left err -> fail . to $ err
  Right res -> return res

-- |
-- Post-process the result of a parser with a possible failure.
--
-- In case of failure the cursor gets positioned
-- in the beginning of the parsed input.
refine :: (Stream s, Ord e) => (a -> Either Text b) -> Parsec e s a -> Parsec e s b
refine refiner parser =
  do
    initialState <- getParserState
    parserResult <- parser
    case refiner parserResult of
      Right res -> return res
      Left err -> do
        setParserState initialState
        fail . to $ err

sepUpdate :: (Stream s, Ord e) => state -> Parsec e s sep -> (state -> Parsec e s state) -> Parsec e s state
sepUpdate state sepP elemP =
  sepUpdate1 state sepP elemP <|> pure state

sepUpdate1 :: (Stream s, Ord e) => state -> Parsec e s sep -> (state -> Parsec e s state) -> Parsec e s state
sepUpdate1 state sepP elemP = do
  state <- elemP state
  let go !state =
        asum
          [ do
              sepP
              state <- elemP state
              go state,
            return state
          ]
   in go state

sepEndUpdate :: (Stream s, Ord e) => state -> Parsec e s sep -> (state -> Parsec e s end) -> (state -> Parsec e s state) -> Parsec e s end
sepEndUpdate state sepP endP elemP =
  asum
    [ endP state,
      do
        state <- elemP state
        let go !state =
              asum
                [ do
                    sepP
                    state <- elemP state
                    go state,
                  endP state
                ]
         in go state
    ]

-- |
-- Associate the result of parsing with an input region.
locate :: (Stream s, Ord e) => Parsec e s res -> Parsec e s (Located.Located res)
locate p = do
  initialPos <- stateOffset <$> getParserState
  res <- p
  finalPos <- stateOffset <$> getParserState
  return $ Located.Located initialPos finalPos res

-- |
-- Associate the result of parsing with an input region.
locate' :: (Ord e) => Parsec e Text res -> Parsec e Text (Parsed.Parsed res)
locate' p = do
  location <- locate p
  input <- getInput
  return $ Parsed.Parsed input location

-- * --

-- |
-- A grouping of the head-tail pattern.
-- Where there is a parser that determines the choice producing a continuation
-- which does not participate in the choice.
altTry :: MonadParsec e s m => [m (m a)] -> m a
altTry =
  join . msum . fmap try
