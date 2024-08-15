module Coalmine.Special where

import Coalmine.InternalPrelude

class Special a where
  -- | Value which the subject value forms a specialized view of.
  type GeneralizationOf a

  -- | Data structure describing the error.
  --
  -- E.g., it may be an ADT if you want to optimize for flexility,
  -- 'Text' if you just want to describe the error or
  -- unit if you don\'t need to provide any extra information when failing.
  type SpecializationErrorOf a

  specialize :: GeneralizationOf a -> Either (SpecializationErrorOf a) a
  generalize :: a -> GeneralizationOf a

-- |
-- Prism from general to specialized value.
--
-- Provides a way to access and modify a general value by focusing on its specialized projection when it\'s possible.
--
-- It\'s Van Laarhoven style, so it\'s compatible with all the optics libraries.
specializedPrism ::
  (Choice p, Applicative f, Special special) =>
  p special (f special) ->
  p (GeneralizationOf special) (f (GeneralizationOf special))
specializedPrism =
  dimap
    (\general -> general & specialize & first (const general))
    (either pure (fmap generalize))
    . right'
