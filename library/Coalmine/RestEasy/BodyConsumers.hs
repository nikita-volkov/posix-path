module Coalmine.RestEasy.BodyConsumers where

import qualified AesonValueParser
import Coalmine.Inter
import Coalmine.InternalPrelude
import Coalmine.Parsing
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser as AesonParser
import qualified Data.Attoparsec.ByteString as AttoparsecByteString
import qualified Data.ByteString as ByteString
import qualified Data.Serialize as Cereal
import qualified Data.Text as Text

type BodyConsumer a =
  IO ByteString -> IO (Either Text a)

refine :: (a -> Either Text b) -> BodyConsumer a -> BodyConsumer b
refine refiner consumer fetch =
  consumer fetch <&> \case
    Right res -> refiner res
    Left err -> Left err

aesonValueParser :: AesonValueParser.Value a -> BodyConsumer a
aesonValueParser parser =
  refine refiner aeson
  where
    refiner = AesonValueParser.runWithTextError parser

aeson :: BodyConsumer Aeson.Value
aeson =
  attoparsecByteStringParser AesonParser.json

attoparsecByteStringParser :: AttoparsecByteString.Parser a -> BodyConsumer a
attoparsecByteStringParser parser fetch =
  fetch >>= AttoparsecByteString.parseWith fetch parser >>= \case
    AttoparsecByteString.Done rmdr res ->
      return $ Right res
    AttoparsecByteString.Fail rmdr contexts msg ->
      return $ Left $ to [j|$contextsBdr: $msg|]
      where
        contextsBdr = foldMap (mappend "/") contexts
    AttoparsecByteString.Partial cont ->
      return $ Left "Not enough input"

cereal :: Cereal.Get a -> BodyConsumer a
cereal get fetch =
  go $ Cereal.runGetPartial get
  where
    go decode = do
      chunk <- fetch
      if ByteString.null chunk
        then return $ Left "Not enough data"
        else case decode chunk of
          Cereal.Done res _ -> return $ Right res
          Cereal.Fail err _ -> return $ Left $ to err
          Cereal.Partial decodeNext -> go decodeNext
