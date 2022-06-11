module Coalmine.RestEasy.BodyConsumers where

import qualified AesonValueParser
import Coalmine.Inter
import Coalmine.InternalPrelude
import Coalmine.Parsing
import qualified Data.Attoparsec.ByteString as AttoparsecByteString
import qualified Data.ByteString as ByteString
import qualified Data.Serialize as Cereal
import qualified Data.Text as Text

type BodyConsumer a =
  IO ByteString -> IO (Either Text a)

aesonValueParser :: AesonValueParser.Value a -> BodyConsumer a
aesonValueParser parser =
  error "TODO"

aesonRefiner refiner =
  error "TODO"

attoparsecByteStringParser :: AttoparsecByteString.Parser a -> BodyConsumer a
attoparsecByteStringParser parser loadChunk =
  loadChunk >>= AttoparsecByteString.parseWith loadChunk parser >>= \case
    AttoparsecByteString.Done rmdr res ->
      return $ Right res
    AttoparsecByteString.Fail rmdr contexts msg ->
      return $ Left $ to [j|$contextsBdr: $msg|]
      where
        contextsBdr = foldMap (mappend "/") contexts
    AttoparsecByteString.Partial cont ->
      return $ Left "Not enough input"

cereal :: Cereal.Get a -> BodyConsumer a
cereal get loadChunk =
  go $ Cereal.runGetPartial get
  where
    go decode = do
      chunk <- loadChunk
      if ByteString.null chunk
        then return $ Left "Not enough data"
        else case decode chunk of
          Cereal.Done res _ -> return $ Right res
          Cereal.Fail err _ -> return $ Left $ to err
          Cereal.Partial decodeNext -> go decodeNext
