module Coalmine.BaseExtras.Applicative where

import Coalmine.InternalPrelude

zipLifting2 :: Applicative f => (forall x. g x -> f x) -> g a1 -> g a2 -> f (a1, a2)
zipLifting2 fn a b = (,) <$> fn a <*> fn b

zipLifting3 :: Applicative f => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> f (a1, a2, a3)
zipLifting3 fn a b c = (,,) <$> fn a <*> fn b <*> fn c

zipLifting4 :: Applicative f => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> g a4 -> f (a1, a2, a3, a4)
zipLifting4 fn a b c d = (,,,) <$> fn a <*> fn b <*> fn c <*> fn d

zipLifting5 :: Applicative f => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> g a4 -> g a5 -> f (a1, a2, a3, a4, a5)
zipLifting5 fn a b c d e = (,,,,) <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e
