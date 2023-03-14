module Coalmine.Result where

data Result context err res
  = FailureResult
      [context]
      -- ^ Path to the error.
      -- List of context choices.
      err
  | OkResult res
