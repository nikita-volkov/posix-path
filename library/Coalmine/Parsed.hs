module Coalmine.Parsed
  ( -- * --
    Parsed (..),
    analyse,
    renderInMegaparsecStyle,

    -- * --
    Scoping,
    runScopingText,
    runScoping,
    scope,
    associate,
    associateEither,
    scopeAndAssociateEither,
    scopeAndAssociateEitherK,
  )
where

import Coalmine.InternalPrelude
import qualified Coalmine.Located as Located

-- * --

-- |
-- Value associated with a snippet of source code.
--
-- Useful for reference-maintaining code analysis.
--
-- A more complete abstraction than 'Located.Located'.
data Parsed a
  = Parsed
      !Text
      -- ^ Reference to the source text.
      !(Located.Located a)
  deriving (Functor, Show, Eq, Foldable, Traversable)

instance Applicative Parsed where
  pure = Parsed "" . pure
  Parsed _ lLoc <*> Parsed rSrc rLoc =
    Parsed rSrc (lLoc <*> rLoc)

instance Comonad Parsed where
  extract (Parsed _ loc) = extract loc
  duplicate (Parsed src loc) = Parsed src (fmap (Parsed src . pure) loc)

-- |
-- Process using a provided pure refinement function.
analyse :: Parsed a -> (a -> Either e b) -> Either (Parsed e) b
analyse (Parsed src loc) mapper =
  first (Parsed src) (Located.analyse loc mapper)

-- |
-- Pretty-print an error message, asssociating it with the input,
-- Megaparsec-style.
renderInMegaparsecStyle :: Parsed Text -> Text
renderInMegaparsecStyle (Parsed input located) =
  Located.renderInMegaparsecStyle located input

-- * --

newtype Scoping e m a
  = Scoping (ExceptT e (StateT (Parsed ()) m) a)
  deriving (Functor, Applicative, Monad, MonadError e)

instance MonadTrans (Scoping e) where
  lift = Scoping . lift . lift

runScopingText :: Functor m => Scoping Text m a -> m (Either Text a)
runScopingText =
  fmap (first renderInMegaparsecStyle) . runScoping

runScoping :: Functor m => Scoping e m a -> m (Either (Parsed e) a)
runScoping (Scoping m) =
  runStateT (runExceptT m) (pure ())
    <&> \(either, parsed) -> first (parsed $>) either

scope :: Monad m => Parsed a -> Scoping e m a
scope parsed =
  Scoping $ put (void parsed) $> extract parsed

-- |
-- Associate a value with the current context
-- by putting it in an associated Parsed.
associate :: Monad m => a -> Scoping e m (Parsed a)
associate =
  Scoping . gets . fmap . const

associateEither :: Monad m => Either e r -> Scoping e m (Parsed r)
associateEither = either throwError associate

scopeAndAssociateEither :: Monad m => Parsed (Either e r) -> Scoping e m (Parsed r)
scopeAndAssociateEither parsed =
  scope parsed >>= associateEither

scopeAndAssociateEitherK :: Monad m => Parsed a -> (a -> Either e b) -> Scoping e m (Parsed b)
scopeAndAssociateEitherK parsed cont =
  scopeAndAssociateEither $ fmap cont parsed
