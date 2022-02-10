-- |
-- A module that reexports only the data types
-- defined across various modules of the \"base\" package.
--
-- By data types we mean that it is the ones we use
-- to define data structures, it is not abstractions.
module Coalmine.BasePreludes.DataTypes
  ( module Exports,
  )
where

import Data.Int as Exports
  ( Int,
    Int16,
    Int32,
    Int64,
    Int8,
  )
import Data.Ratio as Exports
  ( Rational,
  )
import Data.Word as Exports
  ( Word,
    Word16,
    Word32,
    Word64,
    Word8,
  )
import Prelude as Exports
  ( Bool (..),
    Char,
    Double,
    Either (..),
    Float,
    Integer,
    Maybe (..),
    String,
  )
