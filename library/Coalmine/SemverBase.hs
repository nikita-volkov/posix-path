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

import Coalmine.InternalPrelude
import Coalmine.Parsing
import Coalmine.Printing
import qualified Coalmine.TH.QuasiQuoter as QuasiQuoter
import Coalmine.UserErr (UserErr (..))
import qualified Data.Attoparsec.Text as Attoparsec

data SemverBase = SemverBase
  { major :: !Word,
    minor :: !Word,
    patch :: !Word
  }
  deriving (Eq, Ord, Show, Generic, Lift)

instance CompactPrinting SemverBase where
  toCompactBuilder x =
    toCompactBuilder x.major <> "."
      <> toCompactBuilder x.minor
      <> "."
      <> toCompactBuilder x.patch

instance BroadPrinting SemverBase where
  toBroadBuilder = to . toCompactBuilder

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

parseIntList :: [Int] -> Either UserErr SemverBase
parseIntList =
  error "TODO"
