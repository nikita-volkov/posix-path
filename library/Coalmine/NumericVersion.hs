module Coalmine.NumericVersion
  ( NumericVersion,
    parts,
    bump,
  )
where

import Coalmine.InternalPrelude
import Coalmine.Parsing
import Coalmine.Printing
import qualified Data.Attoparsec.Text as Attoparsec

data NumericVersion = NumericVersion
  { head :: !Word,
    tail :: ![Word]
  }
  deriving (Eq, Show, Generic)

instance Ord NumericVersion where
  compare l r =
    compare (parts l) (parts r)

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

parts :: NumericVersion -> [Word]
parts (NumericVersion head tail) =
  head : tail

-- | Bump the version at the specified position if such position exists.
bump :: Int -> NumericVersion -> Maybe NumericVersion
bump position (NumericVersion head tail) =
  case position of
    0 -> Just $ NumericVersion (succ head) []
    _ -> case splitAt (pred position) tail of
      (prefix, suffix) ->
        case suffix of
          x : _ -> Just $ NumericVersion head (prefix <> [succ x])
          _ -> Nothing
