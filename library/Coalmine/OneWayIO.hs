-- |
-- IO that implies that its failing should short-circuit the application.
--
-- IOW, the actions here are not meant to be caught exceptions from.
--
-- This perspective lets us completely avoid exceptions.
module Coalmine.OneWayIO where

import qualified Coalmine.BaseExtras.List as List
import Coalmine.EvenSimplerPaths (Path)
import Coalmine.Inter
import Coalmine.InternalPrelude
import Coalmine.Printing
import qualified Data.ByteString as ByteString

-- * --

-- | Read one of files.
readOneOf :: [Path] -> IO ByteString
readOneOf = go []
  where
    go !errs = \case
      path : tail ->
        catch
          (ByteString.readFile (printCompactAsString path))
          (\e -> go ((e, path) : errs) tail)
      [] ->
        die (from @TextBuilder report)
        where
          report =
            "Failed to read from the following files for the following reasons:\n"
              <> list
            where
              list = List.intercalateMap errReport "\n" errs
              errReport (err, path) =
                "- " <> printCompactAs path <> ": "
                  <> (to . displayException @IOException) err
