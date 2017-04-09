{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}

module Network.HTTP.WebSub.Subscriber
  ( SubscribeError(..)
  , Client(..)
  , Subscriptions
  , newSubscriptions
  , subscribe
  , awaitActiveSubscription
  , deny
  , verify
  , distributeContent
  ) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad (forever)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (maybe, catMaybes)
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
  | VerificationFailed
  | UnexpectedError LBS.ByteString
  deriving (Show, Eq, Ord)

class Client c where
  requestSubscription :: c
                      -> Hub
                      -> SubscriptionRequest
                      -> ExceptT SubscribeError IO ()
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
          SubscriptionRequest -> Chan ContentDistribution -> Subscription Active

data Subscriptions c = Subscriptions
  { baseUri :: URI
  , client :: c
  , pending :: MVar (HashMap CallbackURI (Subscription Pending))
  , active :: MVar (HashMap CallbackURI (Subscription Active))
  }

instance Client c =>
         Client (Subscriptions c) where
  requestSubscription = requestSubscription . client
  getHubLinks = getHubLinks . client

newSubscriptions :: URI -> c -> IO (Subscriptions c)
newSubscriptions baseUri client =
  Subscriptions baseUri client <$> newMVar HM.empty <*> newMVar HM.empty

createPending
  :: Subscriptions c
  -> CallbackURI
  -> SubscriptionRequest
  -> ExceptT SubscribeError IO ()
createPending subscriptions callbackUri req = do
  ready <- lift newEmptyMVar
  lift $
    modifyMVar_
      (pending subscriptions)
      (return . HM.insert callbackUri (Pending req ready))

type ContentDistributionCallback = ContentDistribution -> IO ()

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
  => Subscriptions c -> Hub -> Topic -> IO (Either SubscribeError ())
subscribe subscriptions hub topic =
  runExceptT $ do
    let callbackUri = CallbackURI (baseUri subscriptions)
        subReq = SubscriptionRequest callbackUri Subscribe topic 3600
    requestSubscription (client subscriptions) hub subReq
    createPending subscriptions callbackUri subReq

awaitActiveSubscription
  :: Client c
  => Subscriptions c
  -> CallbackURI
  -> IO (Either SubscribeError (Chan ContentDistribution))
awaitActiveSubscription subscriptions callbackUri =
  runExceptT $ do
    Pending _ pendingResult <-
      do pending <- lift (findPendingSubscription subscriptions callbackUri)
         maybe
           (throwError (UnexpectedError "Pending subscription not found."))
           return
           pending
    Active _ notifications <- ExceptT (readMVar pendingResult)
    return notifications

deny :: Subscriptions c -> CallbackURI -> Denial -> IO Bool
deny subscriptions callbackUri denial =
  findPendingSubscription subscriptions callbackUri >>= \case
    Just (Pending _ result) ->
      putMVar result (Left (SubscriptionDenied denial)) *> return True
    Nothing -> return False

verify :: Subscriptions c -> CallbackURI -> VerificationRequest -> IO Bool
verify subscriptions callbackUri VerificationRequest {topic = verTopic} =
  findPendingSubscription subscriptions callbackUri >>= \case
    Just (Pending subReq@SubscriptionRequest {topic = subTopic} result)
      | verTopic == subTopic -> do
        notifications <- newChan
        putMVar result (Right (Active subReq notifications))
        return True
      | otherwise -> do
        putMVar result (Left VerificationFailed)
        return False
    Nothing -> return False

distributeContent :: Subscriptions c -> CallbackURI -> ContentDistribution -> IO Bool
distributeContent subscriptions callbackUri notification =
  findActiveSubscription subscriptions callbackUri >>= \case
    Just (Active _ chan) -> writeChan chan notification *> return True
    Nothing -> return False
