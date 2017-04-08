{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.HTTP.WebSub.Subscriber
  ( Client
  , newClient
  , subscribe
  , getHubLinks
  , notify
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types
       (Link(..), LinkParam(..), linkParams)
import Network.HTTP.Media.MediaType (MediaType)
import Network.HTTP.Simple as HTTP
import Network.HTTP.Types.Status (status202)
import Network.HTTP.WebSub
import Network.URI
       (URI, uriAuthority, uriRegName, uriScheme, uriToString)

import Web.FormUrlEncoded

data SubscriptionError
  = ValidationFailed
  | VerificationFailed

data Subscription
  = Pending SubscriptionRequest (MVar ())
  | Failed  SubscriptionRequest SubscriptionError
  | Verified SubscriptionRequest
             (Chan Notification)

data Client = Client
  { baseUri :: URI
  , subscribers :: MVar (HashMap CallbackURI Subscription)
  }

newClient :: URI -> IO Client
newClient baseUri = Client baseUri <$> newMVar HM.empty

insertSubscription :: Client -> CallbackURI -> Subscription -> IO ()
insertSubscription client callbackUri subscription =
  modifyMVar_ (subscribers client) (return . HM.insert callbackUri subscription)

findSubscription :: Client -> CallbackURI -> IO (Maybe Subscription)
findSubscription client uri = HM.lookup uri <$> readMVar (subscribers client)

createPending :: Client
              -> CallbackURI
              -> SubscriptionRequest
              -> IO (MVar ())
createPending client callbackUri req = do
  ready <- newEmptyMVar
  insertSubscription client callbackUri (Pending req ready)
  return ready

createSubscriptionChan :: Client
                       -> CallbackURI
                       -> SubscriptionRequest
                       -> IO (Chan Notification)
createSubscriptionChan client callbackUri req = do
  chan <- newChan
  insertSubscription client callbackUri (Verified req chan)
  return chan

data SubscribeError
  = InvalidHub Hub
  | HTTPError (HTTP.Response LBS.ByteString)

requestFromUri :: URI -> Maybe Request
requestFromUri uri
  -- Hacky way of going from URI to Request, by rendering as String and
  -- parsing it as a Request.
 = parseRequest (uriToString id uri "")

requestSubscription :: Client
                    -> Hub
                    -> SubscriptionRequest
                    -> IO (Either SubscribeError ())
requestSubscription client hub subReq =
  case makeHubRequest hub subReq of
    Just req -> do
      res <- httpLBS req
      if getResponseStatus res == status202
        then return (Right ())
        else return (Left (HTTPError res))
    Nothing -> return (Left (InvalidHub hub))
  where
    makeHubRequest (Hub hub) subReq =
      setRequestMethod "POST" . setRequestBodyLBS (urlEncodeAsForm subReq) <$>
      requestFromUri hub

type NotificationCallback = Notification -> IO ()

subscribe :: Client -> Hub -> Topic -> NotificationCallback -> IO ()
subscribe client hub topic onNotification = do
  let callbackUri = CallbackURI (baseUri client)
      subReq = SubscriptionRequest callbackUri Subscribe topic
  requestSubscription client hub subReq
  ready <- createPending client callbackUri subReq
  readMVar ready
  -- chan <- createSubscriptionChan client callbackUri subReq
  -- forkIO $ forever (readChan chan >>= onNotification)
  return ()

getHubLinks :: Client -> Topic -> IO [Hub]
getHubLinks client (Topic uri) =
  case setRequestMethod "HEAD" <$> requestFromUri uri of
    Just req -> do
      res <- httpNoBody req
      return (hubLinks (getResponseHeader "Link" res))
    Nothing -> return []
  where
    isHubLink link = lookup Rel (linkParams link) == Just "hub"
    hubLinks :: [C.ByteString] -> [Hub]
    hubLinks headers =
      headers & map parseLinkHeaderBS & catMaybes & concat & filter isHubLink &
      map toHub
    toHub (Link uri _) = Hub uri

data NotifyError
  = SubscriptionNotFound
  | SubscriptionFailed SubscriptionRequest SubscriptionError

notify :: Client -> CallbackURI -> Notification -> IO (Either NotifyError ())
notify client callbackUri notification =
  findSubscription client callbackUri >>= \case
    Just (Verified _ chan) -> writeChan chan notification *> return (Right ())
    Just (Failed req err) -> return (Left (SubscriptionFailed req err))
    Nothing -> return (Left SubscriptionNotFound)
