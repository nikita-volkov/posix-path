module Coalmine.Name.Attoparsec where

import Coalmine.AttoparsecExtras.Text
import Coalmine.InternalPrelude
import Coalmine.Name.Charsets qualified as Charsets
import Data.Attoparsec.Text hiding (sepBy, sepBy1)
import VectorBuilder.MonadPlus

complete :: Parser a -> Parser a
complete parser = parser <* endOfInput

parts :: Parser (BVec Text)
parts = sepBy1 part (char '-')

part :: Parser Text
part = textOfCharset1 Charsets.part
