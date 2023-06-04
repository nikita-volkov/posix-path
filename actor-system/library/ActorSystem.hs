module ActorSystem where

import Coalmine.Prelude

-- | Context, in which actors are hired and arranged.
data ActorSystem a

-- | Block running the actor system until any of its actors throw an exception.
runActorSystem :: ActorSystem () -> IO ()
runActorSystem =
  error "TODO"

-- | Handle to a thread that executes a machine.
data Actor cmd = Actor
  { queue :: TBQueue cmd
  }

spawn ::
  -- | Initialization procedure.
  -- Use it to acquire process-wide resources.
  IO state ->
  -- | Step function that handles commands.
  -- Throwing exceptions will terminate the running actor system and get escalated.
  -- All other actors will be killed.
  (cmd -> state -> IO state) ->
  -- | Clean up procedure.
  -- For releasing resources when an exception gets thrown by this actor
  -- or when the actor system terminates due to another actor throwing an exception.
  (state -> IO ()) ->
  ActorSystem (Actor cmd)
spawn =
  error "TODO"

command :: Actor cmd -> cmd -> IO ()
command =
  error "TODO"
