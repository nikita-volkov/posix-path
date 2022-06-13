module Coalmine.EvenSimplerPaths.Charsets where

import Coalmine.InternalPrelude
import StructureKit.Charset

-- https://stackoverflow.com/a/35352640/485115
notFileName :: Charset
notFileName = "./\\:\NUL*?\"<>|"
