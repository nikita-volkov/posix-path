module Coalmine.Inter.Analysis where

import Coalmine.Inter.Format.Model
import Coalmine.Prelude

spacesIndentation :: BVec Space -> Int
spacesIndentation = getSum . foldMap (Sum . spaceIndentation)

spaceIndentation :: Space -> Int
spaceIndentation = \case
  SpaceSpace -> 1
  TabSpace -> 2

lineIndentedSegments :: Line -> (Int, BVec ContentSegment)
lineIndentedSegments =
  (,) <$> spacesIndentation . #indentation <*> #content

quasiQuoteIndentedSegments :: QuasiQuote -> BVec (Int, BVec ContentSegment)
quasiQuoteIndentedSegments =
  fmap lineIndentedSegments . #content
