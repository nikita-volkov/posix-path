module Coalmine.Result where

import Coalmine.InternalPrelude

data Result context err res
  = FailureResult
      [context]
      -- ^ Path to the error.
      -- List of context choices.
      err
  | OkResult res
