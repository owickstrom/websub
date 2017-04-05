{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.WebSub.Subscriber
       ( Client
       , newClient
       , subscribe
       , notify
       ) where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad                (forever)

import qualified Data.ByteString.Char8        as C
import qualified Data.ByteString.Lazy         as LBS
import           Data.Function                ((&))
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HM

import           Network.HTTP.Media.MediaType (MediaType)
import           Network.HTTP.Simple          as HTTP
import           Network.HTTP.Types.Status    (status202)
import           Network.HTTP.WebSub

import           Network.URI                  (URI, uriAuthority, uriRegName,
                                               uriScheme)

newtype Client
  = Client { subscribers :: MVar (HashMap CallbackURI (Chan Notification))
           }


newClient :: IO Client
newClient =
  Client <$> newMVar HM.empty


createSubscriberChan :: Client
                     -> CallbackURI
                     -> IO (Chan Notification)
createSubscriberChan client callbackUri = do
  chan <- newChan
  modifyMVar_ (subscribers client) (return . HM.insert callbackUri chan)
  return chan


findSubscriberChan :: Client
                   -> CallbackURI
                   -> IO (Maybe (Chan Notification))
findSubscriberChan client uri = do
  chans <- readMVar (subscribers client)
  return (HM.lookup uri chans)


data SubscribeError
  = InvalidHub Hub
  | HTTPError (HTTP.Response LBS.ByteString)


makeHubRequest :: Hub -> Maybe HTTP.Request
makeHubRequest (Hub hub) = do
  auth <- uriAuthority hub
  return $
    defaultRequest
    & setRequestMethod "POST"
    & setRequestSecure (uriScheme hub == "https:")
    & setRequestHost (C.pack (uriRegName auth))


requestSubscription :: Client
                    -> Hub
                    -> SubscriptionRequest
                    -> IO (Either SubscribeError ())
requestSubscription client hub subscriptionReq =
  case makeHubRequest hub of
    Just req -> do
      res <- httpLBS req
      if getResponseStatus res == status202
         then return (Right ())
         else return (Left (HTTPError res))
    Nothing   -> return (Left (InvalidHub hub))


type NotificationCallback = Notification -> IO ()


addNotificationCallback :: Client
                        -> Subscriber
                        -> NotificationCallback
                        -> IO ()
addNotificationCallback client subscriber callback =
  return ()


subscribe :: Client
          -> Hub
          -> SubscriptionRequest
          -> NotificationCallback
          -> IO ()
subscribe client hub req onNotification = do
  chan <- createSubscriberChan client (callbackURI (subscriber req))
  -- requestSubscription client hub req
  forkIO $ forever (readChan chan >>= onNotification)
  return ()


notify :: Client
       -> CallbackURI
       -> Notification
       -> IO Bool
notify client callbackUri notification = do
  c <- findSubscriberChan client callbackUri
  case c of
    Just chan -> do
      writeChan chan notification
      return True
    Nothing ->
      return False
