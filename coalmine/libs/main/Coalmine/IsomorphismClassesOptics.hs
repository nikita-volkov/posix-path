module Coalmine.IsomorphismClassesOptics where

import Coalmine.InternalPrelude
import Coalmine.IsomorphismClasses
import Optics

frozen :: (Freezing melted frozen) => Iso' melted frozen
frozen = iso freeze melt

melted :: (Freezing melted frozen) => Iso' frozen melted
melted = iso melt freeze

strict :: (Strictness lazy strict) => Iso' lazy strict
strict = iso toStrict toLazy

lazy :: (Strictness lazy strict) => Iso' strict lazy
lazy = iso toLazy toStrict
