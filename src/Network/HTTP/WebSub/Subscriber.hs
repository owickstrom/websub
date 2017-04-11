{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}

module Network.HTTP.WebSub.Subscriber
  ( SubscriptionId(..)
  , SubscribeError(..)
  , ContentDistributionCallback
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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (maybe, catMaybes)
import Data.Text (Text)

import Data.Hashable
import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types
       (Link(..), LinkParam(..), linkParams)
import Network.HTTP.Media.MediaType (MediaType)
import Network.HTTP.Simple as HTTP
import Network.HTTP.Types.Status (status202)
import Network.HTTP.WebSub
import Network.URI

import Web.FormUrlEncoded

newtype SubscriptionId =
  SubscriptionId BS.ByteString
  deriving (Show, Eq)

instance Hashable SubscriptionId where
  hashWithSalt salt = hashWithSalt salt . show

data SubscribeError
  = InvalidHub Hub
  | SubscriptionDenied Denial
  | VerificationFailed
  | UnexpectedError LBS.ByteString
  deriving (Show, Eq, Ord)

class Client c where
  requestSubscription ::
       c -> Hub -> SubscriptionRequest -> ExceptT SubscribeError IO ()
  getHubLinks :: c -> Topic -> IO [Hub]

type ContentDistributionCallback = ContentDistribution -> IO ()

data Pending =
  Pending SubscriptionRequest
          ContentDistributionCallback
          (MVar (Either SubscribeError Subscription))

data Subscription =
  Subscription SubscriptionRequest
               ContentDistributionCallback

instance Show Subscription where
  show (Subscription req _) = "(Subscription " ++ show req ++ " ...)"

data Subscriptions c = Subscriptions
  { baseUri :: URI
  , client :: c
  , pending :: MVar (HashMap SubscriptionId Pending)
  , active :: MVar (HashMap SubscriptionId Subscription)
  }

instance Client c => Client (Subscriptions c) where
  requestSubscription = requestSubscription . client
  getHubLinks = getHubLinks . client

newSubscriptions :: URI -> c -> IO (Subscriptions c)
newSubscriptions baseUri client =
  Subscriptions baseUri client <$> newMVar HM.empty <*> newMVar HM.empty

randomIdStr :: IO String
randomIdStr = return "foo"

createPending ::
     Subscriptions c
  -> SubscriptionId
  -> SubscriptionRequest
  -> ContentDistributionCallback
  -> ExceptT SubscribeError IO ()
createPending subscriptions subscriptionId req callback = do
  ready <- lift newEmptyMVar
  lift $
    modifyMVar_
      (pending subscriptions)
      (return . HM.insert subscriptionId (Pending req callback ready))

activate :: Subscriptions c -> SubscriptionId -> Subscription -> IO ()
activate subscriptions subscriptionId subscription =
  modifyMVar_
    (active subscriptions)
    (return . HM.insert subscriptionId subscription)

findPendingSubscription ::
     Subscriptions c -> SubscriptionId -> IO (Maybe Pending)
findPendingSubscription subscriptions subscriptionId =
  HM.lookup subscriptionId <$> readMVar (pending subscriptions)

findActiveSubscription ::
     Subscriptions c -> SubscriptionId -> IO (Maybe Subscription)
findActiveSubscription subscriptions subscriptionId =
  HM.lookup subscriptionId <$> readMVar (active subscriptions)

subscribe ::
     Client c
  => Subscriptions c
  -> Hub
  -> Topic
  -> ContentDistributionCallback
  -> IO (Either SubscribeError SubscriptionId)
subscribe subscriptions hub topic callback =
  runExceptT $ do
    idStr <- lift randomIdStr
    let base = baseUri subscriptions
        callbackUri =
          CallbackURI (base {uriPath = uriPath base ++ "/" ++ idStr})
        subscriptionId = SubscriptionId (C.pack idStr)
        subReq = SubscriptionRequest callbackUri Subscribe topic 3600
    -- First create a pending subscription.
    createPending subscriptions subscriptionId subReq callback
    -- Then request the subscription from the hub. The hub might
    -- synchronously validate and verify the subscription, thus the
    -- subscription has to be created and added as pending before.
    requestSubscription (client subscriptions) hub subReq
    return subscriptionId

awaitActiveSubscription ::
     Client c
  => Subscriptions c
  -> SubscriptionId
  -> IO (Either SubscribeError ())
awaitActiveSubscription subscriptions subscriptionId =
  runExceptT $ do
    Pending _ _ pendingResult <-
      do pending <- lift (findPendingSubscription subscriptions subscriptionId)
         maybe
           (throwError (UnexpectedError "Pending subscription not found."))
           return
           pending
    Subscription _ callback <- ExceptT (readMVar pendingResult)
    lift $ removePending subscriptions subscriptionId
    return ()

removePending :: Subscriptions c -> SubscriptionId -> IO ()
removePending subscriptions subscriptionId =
  modifyMVar_ (pending subscriptions) (return . HM.delete subscriptionId)

deny :: Subscriptions c -> SubscriptionId -> Denial -> IO Bool
deny subscriptions subscriptionId denial =
  findPendingSubscription subscriptions subscriptionId >>= \case
    Just (Pending _ _ result) ->
      putMVar result (Left (SubscriptionDenied denial)) *> return True
    Nothing -> return False

verify :: Subscriptions c -> SubscriptionId -> VerificationRequest -> IO Bool
verify subscriptions subscriptionId VerificationRequest {topic = verTopic} =
  findPendingSubscription subscriptions subscriptionId >>= \case
    Just (Pending subReq@SubscriptionRequest {topic = subTopic} callback result)
      | verTopic == subTopic -> do
        let sub = Subscription subReq callback
        activate subscriptions subscriptionId sub
        putMVar result (Right sub)
        return True
      | otherwise -> do
        putMVar result (Left VerificationFailed)
        return False
    Nothing -> return False

distributeContent ::
     Subscriptions c -> SubscriptionId -> ContentDistribution -> IO Bool
distributeContent subscriptions subscriptionId notification =
  findActiveSubscription subscriptions subscriptionId >>= \case
    Just (Subscription _ callback) -> callback notification *> return True
    Nothing -> return False
