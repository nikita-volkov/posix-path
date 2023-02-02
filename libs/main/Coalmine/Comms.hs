module Coalmine.Comms
  ( serializeAsByteStringWithSchema,
    deserializeByteString,
    Serialization (..),
  )
where

import Coalmine.Comms.Schema qualified as Schema
import Coalmine.InternalPrelude
import Coalmine.PtrKit.Reader qualified as Reader
import Coalmine.PtrKit.Streamer qualified as Streamer
import Coalmine.PtrKit.Writer qualified as Writer

serializeAsByteStringWithSchema :: Serialization a => a -> ByteString
serializeAsByteStringWithSchema =
  error "TODO"

deserializeByteString :: Serialization a => ByteString -> Either Text a
deserializeByteString =
  error "TODO"

-- |
-- Lawful serialization typeclass.
--
-- The laws are:
-- - Serializing using any method and deserializing must produce the original value.
-- - Serializing using all methods must be convertible to identical bytestrings.
-- - Serializing a value, then deserializing a dynamically typed value using schema,
-- then serializing the dynamic value, must produce the same bytestring as the one
-- after the first serialization.
class Serialization a where
  schema :: Schema a

  -- | Serialize as stream.
  serializeAsStreamer :: a -> Streamer.Streamer

  -- | Serialize in whole.
  serializeAsWriter :: a -> Writer.Writer

  deserialize :: Reader.Reader a

newtype Schema a = Schema Schema.Schema
