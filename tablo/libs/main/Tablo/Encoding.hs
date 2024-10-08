module Tablo.Encoding
  ( -- * Execution
    encodeRowsAsTsvLazyByteString,
    encodeRowAsTsvLazyByteString,

    -- * Encoder construction DSL
    TableEncoder,
    column,
    CellEncoder,
    text,
    integer,
    fixedDouble,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Data.Csv qualified as Cassava
import Data.Csv.Builder qualified as Cassava.Builder
import Data.Function
import Data.Functor.Contravariant
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import Tablo.Encoding.Cassava.EncodeOptions qualified as Cassava.EncodeOptions
import Text.Printf
import Prelude

encodeRowsAsTsvLazyByteString ::
  (Foldable f) =>
  -- | Include header.
  Bool ->
  TableEncoder row ->
  f row ->
  LazyByteString
encodeRowsAsTsvLazyByteString includeHeader encoder rows =
  rows
    & encodeUsingCassava (Cassava.EncodeOptions.tsv includeHeader) encoder
    & Builder.toLazyByteString

encodeUsingCassava :: (Foldable f) => Cassava.EncodeOptions -> TableEncoder row -> f row -> Builder
encodeUsingCassava options encoder rows =
  Cassava.Builder.encodeHeaderWith options encoder.header
    <> foldMap (encodeRowUsingCassava options encoder) rows

encodeRowUsingCassava :: Cassava.EncodeOptions -> TableEncoder row -> row -> Builder
encodeRowUsingCassava options TableEncoder {..} row =
  columnEncoders
    & fmap ($ row)
    & Cassava.Builder.encodeRecordWith options

encodeRowAsTsvLazyByteString :: TableEncoder row -> row -> LazyByteString
encodeRowAsTsvLazyByteString encoder =
  Builder.toLazyByteString
    . encodeRowUsingCassava (Cassava.EncodeOptions.tsv False) encoder

data TableEncoder a
  = TableEncoder
  { header :: Vector.Vector ByteString,
    columnEncoders :: Vector.Vector (a -> ByteString)
  }

instance Semigroup (TableEncoder a) where
  l <> r =
    TableEncoder
      { header = l.header <> r.header,
        columnEncoders = l.columnEncoders <> r.columnEncoders
      }

instance Monoid (TableEncoder a) where
  mempty =
    TableEncoder
      { header = mempty,
        columnEncoders = mempty
      }

column ::
  -- | Header.
  Text ->
  -- | Field accessor.
  --
  -- When 'Nothing' the cell is empty.
  (row -> Maybe cell) ->
  -- | Encoder.
  CellEncoder cell ->
  TableEncoder row
column header extractor cellEncoder =
  TableEncoder
    { header =
        pure (Text.encodeUtf8 header),
      columnEncoders =
        pure \row ->
          extractor row
            & fmap cellEncoder.encode
            & fromMaybe mempty
    }

newtype CellEncoder a = CellEncoder
  { encode :: a -> ByteString
  }
  deriving
    (Contravariant)
    via (Op ByteString)

text :: CellEncoder Text
text =
  CellEncoder Cassava.toField

integer :: CellEncoder Int
integer =
  CellEncoder Cassava.toField

-- | Double rendered as decimal with a fixed amount of digits after the point.
fixedDouble ::
  -- | Digits after point.
  Int ->
  CellEncoder Double
fixedDouble digitsAfterPoint =
  CellEncoder (Cassava.toField @String . printf ("%." ++ show digitsAfterPoint ++ "f"))
