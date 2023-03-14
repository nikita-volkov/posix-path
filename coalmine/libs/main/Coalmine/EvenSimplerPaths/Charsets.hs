module Coalmine.EvenSimplerPaths.Charsets where

import StructureKit.Charset

-- https://stackoverflow.com/a/35352640/485115
notFileName :: Charset
notFileName = "./\\:\NUL*?\"<>|"
