{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Network.HTTP.WebSub.Subscriber
  ( SubscribeError(..)
  , Client(..)
  , Subscriptions
  , newSubscriptions
  , subscribe
  , awaitActiveSubscription
  , deny
  , distributeContent
  ) where

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
  | SubscriptionDenied Denial
  | UnexpectedError LBS.ByteString
  deriving (Show, Eq, Ord)

class Client c where
  requestSubscription :: c
                      -> Hub
                      -> SubscriptionRequest
                      -> IO (Either SubscribeError ())
  getHubLinks :: c -> Topic -> IO [Hub]

data Pending

data Denied

data Active

data Subscription s where
        Pending ::
          SubscriptionRequest ->
            MVar (Either SubscribeError (Subscription Active)) ->
              Subscription Pending
        Denied :: SubscriptionRequest -> Subscription Denied
        Active ::
          SubscriptionRequest -> Chan Notification -> Subscription Active

data Subscriptions c = Subscriptions
  { baseUri :: URI
  , client :: c
  , pending :: MVar (HashMap CallbackURI (Subscription Pending))
  , active :: MVar (HashMap CallbackURI (Subscription Active))
  }

instance Client c => Client (Subscriptions c) where
  requestSubscription = requestSubscription . client
  getHubLinks = getHubLinks . client

newSubscriptions :: URI -> c -> IO (Subscriptions c)
newSubscriptions baseUri client =
  Subscriptions baseUri client <$> newMVar HM.empty <*> newMVar HM.empty

createPending
  :: Subscriptions c
  -> CallbackURI
  -> SubscriptionRequest
  -> IO ()
createPending subscriptions callbackUri req = do
  ready <- newEmptyMVar
  insertSubscription subscriptions callbackUri (Pending req ready)
  where
    insertSubscription subscriptions callbackUri subscription =
      modifyMVar_
        (pending subscriptions)
        (return . HM.insert callbackUri subscription)

type NotificationCallback = Notification -> IO ()

findPendingSubscription :: Subscriptions c
                        -> CallbackURI
                        -> IO (Maybe (Subscription Pending))
findPendingSubscription subscriptions uri =
  HM.lookup uri <$> readMVar (pending subscriptions)

findActiveSubscription :: Subscriptions c
                       -> CallbackURI
                       -> IO (Maybe (Subscription Active))
findActiveSubscription subscriptions uri =
  HM.lookup uri <$> readMVar (active subscriptions)

subscribe
  :: Client c
  => Subscriptions c
  -> Hub
  -> Topic
  -> IO (Either SubscribeError ())
subscribe subscriptions hub topic = do
  let callbackUri = CallbackURI (baseUri subscriptions)
      subReq = SubscriptionRequest callbackUri Subscribe topic
  requestSubscription (client subscriptions) hub subReq >>=
    \case
      Left err -> return (Left err)
      Right () -> do
        putStrLn "Creating pending subscription."
        -- Create and await the transition from 'Pending' to 'Failed' or 'Active'.
        createPending subscriptions callbackUri subReq
        return (Right ())
  where
    createSubscriptionChan subscriptions callbackUri req = do
      chan <- newChan
      insertSubscription subscriptions callbackUri (Active req chan)
      return chan
    insertSubscription subscriptions callbackUri subscription =
      modifyMVar_
        (active subscriptions)
        (return . HM.insert callbackUri subscription)

awaitActiveSubscription
  :: Client c
  => Subscriptions c
  -> CallbackURI
  -> IO (Either SubscribeError (Chan Notification))
awaitActiveSubscription subscriptions callbackUri =
  findPendingSubscription subscriptions callbackUri >>=
    \case
      Nothing -> return (Left (UnexpectedError "Pending subscription not found."))
      Just (Pending _ pendingResult) ->
        readMVar pendingResult >>=
           \case
              Left err -> return (Left err)
              Right (Active _ notifications) -> return (Right notifications)

deny :: Subscriptions c -> CallbackURI -> Denial -> IO Bool
deny subscriptions callbackUri denial =
  findPendingSubscription subscriptions callbackUri >>=
    \case
      Just (Pending _ result) ->
        putMVar result (Left (SubscriptionDenied denial)) *> return True
      Nothing -> return False

distributeContent :: Subscriptions c -> CallbackURI -> Notification -> IO Bool
distributeContent subscriptions callbackUri notification =
  findActiveSubscription subscriptions callbackUri >>= \case
    Just (Active _ chan) -> writeChan chan notification *> return True
    Nothing -> return False
