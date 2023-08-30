module Coalmine.Gen where

import Coalmine.InternalPrelude

-- | Pure stateful generator of a stream of values.
--
-- Can be materialized to list or to any applicative program.
--
-- The latter allows it to be used as a controller
-- of applicative consumer effects.
newtype Gen state a = Gen
  { -- | Generate the next value based on a state.
    run :: state -> Maybe (a, state)
  }

deriving instance Functor (Gen state)

instance Applicative (Gen state) where
  pure a = Gen (\state -> Just (a, state))
  Gen runL <*> Gen runR =
    Gen
      ( \state -> case runL state of
          Just (resL, state) -> case runR state of
            Just (resR, state) -> Just (resL resR, state)
            Nothing -> Nothing
          Nothing -> Nothing
      )

-- | Nest the state of a generator in a broader one using the provided optic.
--
-- This allows to make the generator composable with a broader one.
broaden :: (b -> a) -> (a -> b -> b) -> Gen a result -> Gen b result
broaden get set (Gen gen) =
  Gen $ \state ->
    case gen (get state) of
      Just (res, innerState) ->
        Just (res, set innerState state)
      Nothing -> Nothing

-- | Use generator to build a possibly infinite lazy list.
buildList :: Gen state a -> state -> [a]
buildList =
  error "TODO"

-- | Use the generator to control the execution of a program,
-- which is an applicative functor.
controlApplicative :: (Applicative program) => Gen state msg -> state -> (msg -> program ()) -> program ()
controlApplicative (Gen gen) state consume =
  go state
  where
    go state = case gen state of
      Nothing -> pure ()
      Just (msg, state) -> consume msg *> go state
