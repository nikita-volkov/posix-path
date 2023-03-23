-- |
-- Defines an actor model with initialization and usage phases clearly separated.
--
-- First you spawn the actors and only then do you
module ActorSystem where

import Coalmine.Prelude

data ActorSystem

-- | Start an actor system running actors specified using 'Spawn'.
start :: Spawn () -> IO ActorSystem
start =
  error "TODO"

-- | Stop the actor system.
stop :: ActorSystem -> IO ()
stop =
  error "TODO"

data Spawn actor

statefulActor :: state -> (state -> cmd -> IO ()) -> (state -> IO ()) -> Spawn (Actor cmd)
statefulActor =
  error "TODO"

data Actor cmd

tell :: Actor cmd -> cmd -> IO ()
tell =
  error "TODO"
