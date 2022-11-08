module Coalmine.Resilient
  ( Resilient,
    acquire,
    release,
    resiliently,
  )
where

import Coalmine.InternalPrelude

-- | Resilient resource.
data Resilient resource = Resilient
  { -- | Acquired resource.
    acquiredResource :: MVar (Maybe resource),
    -- | Action acquiring the resource with a policy.
    acquire :: IO resource,
    -- | Action releasing the resource.
    release :: resource -> IO ()
  }

acquire :: IO resource -> (resource -> IO ()) -> IO (Resilient resource)
acquire acquire release = do
  acquiredResource <- newMVar Nothing
  return $ Resilient acquiredResource acquire release

release :: Resilient resource -> IO ()
release resilient = do
  resource <- takeMVar resilient.acquiredResource
  case resource of
    Nothing -> do
      putMVar resilient.acquiredResource Nothing
    Just resource -> do
      resilient.release resource
      putMVar resilient.acquiredResource Nothing

resiliently ::
  Resilient resource ->
  -- | Resource handler.
  --
  -- Tries the last acquired resource and if it returns 'Nothing' it gets
  -- reexecuted on a reacquired resource until it produces 'Just'.
  --
  -- If it throws any exception, no changes to the resilient happen. The
  -- exception gets rethrown.
  (resource -> IO (Maybe result)) ->
  IO result
resiliently resilient handler = do
  resource <- takeMVar resilient.acquiredResource
  case resource of
    Nothing -> do
      resource <- resilient.acquire
      tryResource resource
    Just resource -> tryResource resource
  where
    tryResource resource = do
      result <- try @SomeException $ handler resource
      case result of
        Right (Just result) -> do
          putMVar resilient.acquiredResource $ Just resource
          return result
        Right Nothing -> do
          resilient.release resource
          -- TODO: Insert a pause according to policy
          newResource <- resilient.acquire
          tryResource newResource
        Left exception -> do
          putMVar resilient.acquiredResource (Just resource)
          throw exception
