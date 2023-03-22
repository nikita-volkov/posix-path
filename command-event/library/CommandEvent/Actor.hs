module CommandEvent.Actor where

import Coalmine.Prelude

-- |
-- Actor which performs commands and generates events.
--
-- - @cmd@ is a command for the actor to perform.
-- - @event@ is one of the possibly many events generated during the execution of a command.
--
-- Both are supposed to be defined alongside the actor. They form its contract.
-- You connect actors by mapping and filtering these params.
--
-- Another way to look at this abstraction is as on an effectful transducer
-- or stream transformer.
--
-- Inspiration:
--
-- - https://blog.ttulka.com/events-vs-commands-in-ddd/#:~:text=Differences%20between%20Events%20and%20Commands,is%20addressed%20to%20only%20one.
-- - https://tuhrig.de/messages-vs-events-vs-commands/
-- - https://www.youtube.com/watch?v=STKCRSUsyP0
newtype Actor cmd event
  = Actor (cmd -> (event -> IO ()) -> IO ())
