module Coalmine.NumericVersion
  ( NumericVersion,
  )
where

import Coalmine.InternalPrelude
import Coalmine.Parsing
import Coalmine.Printing
import qualified Data.Attoparsec.Text as Attoparsec

data NumericVersion = NumericVersion
  { versionHead :: !Word,
    versionTail :: ![Word]
  }
  deriving (Eq, Show, Generic)

instance Ord NumericVersion where
  compare l r =
    compare (versionParts l) (versionParts r)

instance CompactPrinting NumericVersion where
  toCompactBuilder (NumericVersion h t) =
    toCompactBuilder h
      <> foldMap (mappend "." . toCompactBuilder) t

instance BroadPrinting NumericVersion where
  toBroadBuilder = to . toCompactBuilder

instance ToJSON NumericVersion where
  toJSON = toJSON . printCompactAs @Text

instance ToJSONKey NumericVersion where
  toJSONKey = printCompactAs @Text >$< toJSONKey

instance LenientParser NumericVersion where
  lenientParser = do
    head <- Attoparsec.decimal
    tail <- many tailSegmentParser
    return (NumericVersion head tail)
    where
      tailSegmentParser = do
        Attoparsec.char '.'
        Attoparsec.decimal

versionParts :: NumericVersion -> [Word]
versionParts (NumericVersion head tail) =
  head : tail
