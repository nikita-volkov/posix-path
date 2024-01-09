module Coalmine.SemverBase
  ( SemverBase (..),
    lit,
    parts,
    bumpMajor,
    bumpMinor,
    bumpPatch,
    fromComponents,
  )
where

import AesonValueParser qualified
import Coalmine.InternalPrelude
import Coalmine.Literal
import Coalmine.Printing
import Data.Attoparsec.Text qualified as Attoparsec
import QqExtras qualified as QuasiQuoter

data SemverBase = SemverBase
  { major :: !Word,
    minor :: !Word,
    patch :: !Word
  }
  deriving (Eq, Ord, Show, Generic, Lift)

instance IsString SemverBase where
  fromString =
    either (const (fromComponents 0 0 0)) id
      . parseTextLeniently
      . fromString

instance CompactPrinting SemverBase where
  toCompactBuilder x =
    toCompactBuilder x.major
      <> "."
      <> toCompactBuilder x.minor
      <> "."
      <> toCompactBuilder x.patch

instance BroadPrinting SemverBase where
  toBroadBuilder = to . toCompactBuilder

instance FromJSON SemverBase where
  parseJSON = AesonValueParser.runAsValueParser $ AesonValueParser.string $ AesonValueParser.attoparsedText lenientParser

instance ToJSON SemverBase where
  toJSON = toJSON . printCompactAs @Text

instance ToJSONKey SemverBase where
  toJSONKey = printCompactAs @Text >$< toJSONKey

instance LenientParser SemverBase where
  lenientParser = do
    major <- Attoparsec.decimal
    Attoparsec.char '.'
    minor <- Attoparsec.decimal
    Attoparsec.char '.'
    patch <- Attoparsec.decimal
    return (SemverBase major minor patch)

instance Literal SemverBase where
  literalParser = lenientParser
  literalTextBuilder = toCompactBuilder

lit :: QuasiQuoter
lit =
  QuasiQuoter.lenientLiteral $ Proxy @SemverBase

parts :: SemverBase -> [Word]
parts x =
  [x.major, x.minor, x.patch]

bumpMajor :: SemverBase -> SemverBase
bumpMajor x =
  error "TODO"

bumpMinor :: SemverBase -> SemverBase
bumpMinor x =
  error "TODO"

bumpPatch :: SemverBase -> SemverBase
bumpPatch x =
  error "TODO"

fromComponents :: Word -> Word -> Word -> SemverBase
fromComponents = SemverBase
