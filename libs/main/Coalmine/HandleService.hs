module Coalmine.HandleService where

import Coalmine.InternalPrelude

data HandleService i o = forall handle.
  HandleService
  { start :: IO handle,
    process :: handle -> i -> IO o,
    finish :: handle -> IO ()
  }

-- * HTTP

data HttpRequest

data HttpResponse

runAsHttpServer ::
  Word16 ->
  HandleService HttpRequest HttpResponse ->
  IO ()
runAsHttpServer =
  error "TODO"
