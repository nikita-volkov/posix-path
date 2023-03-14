module Coalmine.Name.Charsets where

import Coalmine.InternalPrelude
import StructureKit.Charset

part :: Charset
part = lowerLatin <> num
