module Company where

import Coalmine.Prelude

-- | Context, in which workers are hired and arranged.
data Company a

-- | Block running the company until any of its workers throw an exception.
runCompany :: Company () -> IO ()
runCompany =
  error "TODO"

-- | Handle to a thread that executes a machine.
data Worker cmd = Worker
  { queue :: TBQueue cmd
  }

hire ::
  -- | Initialization procedure.
  -- Use it to acquire process-wide resources.
  IO state ->
  -- | Step function that handles commands.
  -- Throwing exceptions will bankrupt the company and get escalated.
  -- All other workers will be fired.
  (cmd -> state -> IO state) ->
  -- | Clean up procedure.
  -- For releasing resources when an exception gets thrown by this worker
  -- or when the company bankrupts due to other worker throwing an exception.
  (state -> IO ()) ->
  Company (Worker cmd)
hire =
  error "TODO"

command :: Worker cmd -> cmd -> IO ()
command =
  error "TODO"
