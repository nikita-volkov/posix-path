-- |
-- Search by lines and etc.
module Coalmine.Located.InputAnalysis where

import Coalmine.Prelude
import Data.Text

-- *

-- |
-- Analysis report.
data Analysis

-- *

analyse :: Int -> Int -> Text -> Analysis
analyse start end text =
  error "TODO"
