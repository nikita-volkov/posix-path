-- |
-- A module that reexports only the data types
-- defined across various modules of the \"base\" package.
--
-- By data types we mean that it is the ones we use
-- to define data structures.
-- It is not abstraction integration wrappers,
-- like "Data.Semigroup.First".
-- It is not resource types like "System.IO.Handle".
module Coalmine.BasePreludes.DataTypes
  ( -- * From "Prelude"
    Prelude.Bool (..),
    Prelude.Char,
    Prelude.Double,
    Prelude.Either (..),
    Prelude.Float,
    Prelude.Integer,
    Prelude.Maybe (..),
    Prelude.String,

    -- * From "Data.Int"
    Data.Int.Int,
    Data.Int.Int16,
    Data.Int.Int32,
    Data.Int.Int64,
    Data.Int.Int8,

    -- * From "Data.Word"
    Data.Word.Word,
    Data.Word.Word16,
    Data.Word.Word32,
    Data.Word.Word64,
    Data.Word.Word8,

    -- * From "Data.Ratio"
    Data.Ratio.Rational,

    -- * From "Data.List.NonEmpty"
    Data.List.NonEmpty.NonEmpty (..),
  )
where

import Data.Int
import Data.List.NonEmpty
import Data.Ratio
import Data.Word
import Prelude
