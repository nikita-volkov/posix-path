module Coalmine.EvenSimplerPaths
  ( -- *
    Path,
  )
where

import Coalmine.BaseExtras.MonadPlus
import Coalmine.InternalPrelude hiding (FilePath, Name)
import qualified Data.Attoparsec.Text as Attoparsec
import qualified System.Directory as Directory
import qualified TextBuilderDev as TextBuilderDev

-- *

data Path
  = Path
      !Bool
      -- ^ Is it absolute?
      ![Name]
      -- ^ Components in reverse order.

-- |
-- Structured name of a single component of a path.
data Name
  = Name
      !Text
      -- ^ Name.
      ![Text]
      -- ^ Extensions in reverse order.

-- *

instance Semigroup Path where
  Path _lAbs _lNames <> Path _rAbs _rNames =
    if _rAbs
      then Path _rAbs _rNames
      else Path _lAbs $ _rNames <> _lNames

instance Monoid Path where
  mempty =
    Path False []

instance ToTextBuilder Path where
  toTextBuilder (Path _abs _nodes) =
    if _abs
      then "/" <> _relative
      else _relative
    where
      _relative =
        TextBuilderDev.intercalate "/" . fmap _fromName $ _nodes
      _fromName (Name _name _extensions) =
        foldl'
          (\_output _extension -> _output <> "." <> fromText _extension)
          (fromText _name)
          _extensions

instance ToString Path where
  toString = toString . toText

instance ToText Path where
  toText = toText . toTextBuilder

instance ToJSON Path where
  toJSON = toJSON . toText

instance ToJSONKey Path where
  toJSONKey = contramap toText toJSONKey

instance Show Path where
  show = show . toText

instance LenientParser Path where
  lenientParser = do
    _abs <- Attoparsec.char '/' $> True <|> pure False
    _parents <- foldlMany (flip (:)) [] _node
    error "TODO"
    where
      _node = error "TODO"

instance IsString Path where
  fromString =
    either error id
      . Attoparsec.parseOnly (lenientParser <* Attoparsec.endOfInput)
      . fromString
