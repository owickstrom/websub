{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Network.HTTP.WebSub.Subscriber
  ( Client
  , newClient
  , subscribe
  , getHubLinks
  , distributeContent
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

data SubscribeError
  = InvalidHub Hub
  | SubscriptionDenied
  | UnexpectedError (HTTP.Response LBS.ByteString)

data Pending

data Failed

data Active

data Subscription s where
        Pending ::
          SubscriptionRequest ->
            MVar (Either SubscribeError (Subscription Active)) ->
              Subscription Pending
        Denied :: SubscriptionRequest -> Subscription Failed
        Active ::
          SubscriptionRequest -> Chan Notification -> Subscription Active

data Client = Client
  { baseUri :: URI
  , pending :: MVar (HashMap CallbackURI (Subscription Pending))
  , active :: MVar (HashMap CallbackURI (Subscription Active))
  }

newClient :: URI -> IO Client
newClient baseUri = Client baseUri <$> newMVar HM.empty <*> newMVar HM.empty

createPending
  :: Client
  -> CallbackURI
  -> SubscriptionRequest
  -> IO (MVar (Either SubscribeError (Subscription Active)))
createPending client callbackUri req = do
  ready <- newEmptyMVar
  insertSubscription client callbackUri (Pending req ready)
  return ready
  where
    insertSubscription client callbackUri subscription =
      modifyMVar_ (pending client) (return . HM.insert callbackUri subscription)

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
        else return (Left (UnexpectedError res))
    Nothing -> return (Left (InvalidHub hub))
  where
    makeHubRequest (Hub hub) subReq =
      setRequestMethod "POST" . setRequestBodyLBS (urlEncodeAsForm subReq) <$>
      requestFromUri hub

type NotificationCallback = Notification -> IO ()

findActiveSubscription :: Client
                       -> CallbackURI
                       -> IO (Maybe (Subscription Active))
findActiveSubscription client uri = HM.lookup uri <$> readMVar (active client)

subscribe :: Client
          -> Hub
          -> Topic
          -> IO (Either SubscribeError (Chan Notification))
subscribe client hub topic = do
  let callbackUri = CallbackURI (baseUri client)
      subReq = SubscriptionRequest callbackUri Subscribe topic
  requestSubscription client hub subReq
  -- Create and await the transition from 'Pending' to 'Failed' or 'Active'.
  createPending client callbackUri subReq >>= readMVar >>=
    \case
      Left err -> return (Left err)
      Right (Active _ chan) -> return (Right chan)
  where
    createSubscriptionChan client callbackUri req = do
      chan <- newChan
      insertSubscription client callbackUri (Active req chan)
      return chan
    insertSubscription client callbackUri subscription =
      modifyMVar_ (active client) (return . HM.insert callbackUri subscription)

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

distributeContent :: Client -> CallbackURI -> Notification -> IO Bool
distributeContent client callbackUri notification =
  findActiveSubscription client callbackUri >>= \case
    Just (Active _ chan) -> writeChan chan notification *> return True
    Nothing -> return False
