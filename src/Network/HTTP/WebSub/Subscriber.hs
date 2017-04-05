{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.WebSub.Subscriber
       ( Client
       , newClient
       , subscribe
       , notify
       ) where

import qualified Data.ByteString.Char8        as C
import qualified Data.ByteString.Lazy         as LBS
import           Data.Function                ((&))
import           Network.HTTP.Media.MediaType (MediaType, (//))
import           Network.HTTP.Simple          as HTTP
import           Network.HTTP.Types.Status    (status202)
import           Network.HTTP.WebSub          (Hub (..), Notification,
                                               Subscriber,
                                               SubscriptionMode (..),
                                               SubscriptionRequest, Topic)
import           Network.URI                  (URI, uriAuthority, uriRegName,
                                               uriScheme)

data Client = Client


newClient :: IO Client
newClient = return Client


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
                    -> Subscriber
                    -> IO (Either SubscribeError ())
requestSubscription client hub subscriber =
  case makeHubRequest hub of
    Just req -> do
      res <- httpLBS req
      if getResponseStatus res == status202
         then return (Right ())
         else return (Left (HTTPError res))
    Nothing   -> return (Left (InvalidHub hub))


type NotificationCallback = Topic -> Notification -> IO ()


addNotificationCallback :: Client
                        -> Subscriber
                        -> NotificationCallback
                        -> IO ()
addNotificationCallback client subscriber callback =
  return ()


subscribe :: Client
          -> Hub
          -> Subscriber
          -> NotificationCallback
          -> IO ()
subscribe client hub subscriber onNotification = do
  requestSubscription client hub subscriber
  return ()


notify :: Client
       -> Subscriber
       -> Topic
       -> Notification
       -> IO ()
notify client subscriber topic notification =
  return ()
  -- onNotification topic notification
