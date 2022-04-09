module Coalmine.EvenSimplerPaths
  ( -- *
    Path,
  )
where

import Coalmine.BaseExtras.MonadPlus
import Coalmine.InternalPrelude hiding (FilePath)
import qualified Data.Attoparsec.Text as Attoparsec
import qualified System.Directory as Directory
import qualified TextBuilderDev as TextBuilderDev

-- *

data Path
  = Path
      !Bool
      -- ^ Is it absolute?
      ![Node]
      -- ^ Nodes in reverse order.

data Node
  = Node
      !Text
      -- ^ Name.
      ![Text]
      -- ^ Extensions in reverse order.

-- *

instance Semigroup Path where
  Path _lAbs _lNodes <> Path _rAbs _rNodes =
    if _rAbs
      then Path _rAbs _rNodes
      else Path _lAbs $ _rNodes <> _lNodes

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
        TextBuilderDev.intercalate "/" . fmap _fromNode $ _nodes
      _fromNode (Node _name _extensions) =
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
