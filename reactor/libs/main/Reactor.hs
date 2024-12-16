{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module Reactor where

import Reactor.Model
import Prelude

data Reactor event cmd = forall model. Reactor
  { init :: IO model,
    step :: event -> model -> IO (cmd, model),
    stop :: model -> IO ()
  }

fromModel :: (IsReactorModel a) => Cfg a -> Reactor (Event a) (Cmd a)
fromModel cfg =
  Reactor
    { init = error "TODO",
      step = error "TODO",
      stop = error "TODO"
    }
