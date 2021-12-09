module Coalmine.Ws where

import Coalmine.Prelude
import qualified Network.WebSockets as Ws
import qualified Network.WebSockets.Client as Ws
import qualified Wuss

-- *

data Err
  = ConnectionErr !Ws.ConnectionException
  | TimeoutErr

data Env
  = Env
      !(TBQueue ByteString)
      -- ^ Incoming messages.
      !(TQueue ByteString)
      -- ^ Outgoing messages.
      !(TMVar ShutdownReason)
      -- ^ Shutdown var.

data ShutdownReason
  = FailedShutdownReason !Err
  | ClosedShutdownReason

-- *

acquire :: String -> Word16 -> String -> Int -> IO Env
acquire host port path timeoutSetting = do
  inQueue <- newTBQueueIO 100
  outQueue <- newTQueueIO
  shutdownVar <- newEmptyTMVarIO
  forkIO $ do
    res <- try $
      timeout timeoutSetting $
        Wuss.runSecureClient host (fromIntegral port) path $ \conn -> do
          -- Fork off a receiving thread.
          forkIO $
            let loop = do
                  res <- try $ timeout timeoutSetting $ Ws.receiveData conn
                  case res of
                    Left exc -> do
                      void $ atomically $ tryPutTMVar shutdownVar $ FailedShutdownReason $ ConnectionErr exc
                    Right res -> case res of
                      Nothing ->
                        void $ atomically $ tryPutTMVar shutdownVar $ FailedShutdownReason $ TimeoutErr
                      Just res ->
                        join . atomically . asum $
                          [ writeTBQueue inQueue res $> loop,
                            isEmptyTMVar shutdownVar >>= \case
                              True -> retry
                              False -> return $ return ()
                          ]
             in loop
          let loop = do
                res <- atomically $ Left <$> readTMVar shutdownVar <|> Right <$> readTQueue outQueue
                case res of
                  Right bs -> do
                    res <- try $ timeout timeoutSetting $ Ws.sendTextData conn bs
                    case res of
                      Left exc ->
                        void $ atomically $ tryPutTMVar shutdownVar $ FailedShutdownReason $ ConnectionErr exc
                      Right Nothing ->
                        void $ atomically $ tryPutTMVar shutdownVar $ FailedShutdownReason $ TimeoutErr
                      Right (Just ()) ->
                        loop
                  _ -> return ()
           in loop
    case res of
      Left exc ->
        void $ atomically $ tryPutTMVar shutdownVar $ FailedShutdownReason $ ConnectionErr exc
      Right Nothing ->
        void $ atomically $ tryPutTMVar shutdownVar $ FailedShutdownReason TimeoutErr
      Right (Just ()) ->
        return ()
  return $ Env inQueue outQueue shutdownVar

release :: Env -> IO ()
release (Env _ _ shutdownVar) =
  void $ atomically $ tryPutTMVar shutdownVar ClosedShutdownReason

send :: Env -> ByteString -> IO (Either Err ())
send = error "TODO"

recv :: Env -> IO (Either Err ByteString)
recv = error "TODO"
