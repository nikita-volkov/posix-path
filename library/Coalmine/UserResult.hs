module Coalmine.UserResult
  ( UserResult,
    inContext,
  )
where

import qualified Coalmine.BaseExtras.List as ListExtras
import Coalmine.Inter
import Coalmine.InternalPrelude
import qualified Coalmine.MultilineTextBuilder as Printer
import Coalmine.Name (Name)
import Coalmine.Printing
import Coalmine.UserErr (UserErr (..))
import qualified Coalmine.UserErr as UserErr
import qualified Data.Text as Text

-- |
-- Consider it a 'Maybe' with extended information about the error.
--
-- Use 'pure' to lift a valid value and 'throwError' to fail.
newtype UserResult a
  = UserResult (Either UserErr a)
  deriving (Show, Functor, Applicative, Monad, MonadError UserErr)

-- | Eliminate this layer of abstraction.
run :: UserResult a -> Either UserErr a
run (UserResult run) = run

mapDef :: (Either UserErr a -> Either UserErr b) -> UserResult a -> UserResult b
mapDef mapper (UserResult either) =
  UserResult (mapper either)

inContext :: Name -> UserResult a -> UserResult a
inContext context =
  mapDef $ first (UserErr.addContext context)
