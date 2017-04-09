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
          SubscriptionRequest ->
            Chan ContentDistribution -> Subscription Active

instance Show (Subscription s) where
  show (Pending req _) = "(Pending " ++ show req ++ " ...)"
  show (Denied req) = "(Denied " ++ show req ++ ")"
  show (Active req _) = "(Active " ++ show req ++ " ...)"

data Subscriptions c = Subscriptions
  { baseUri :: URI
  , client :: c
  , pending :: MVar (HashMap SubscriptionId (Subscription Pending))
  , active :: MVar (HashMap SubscriptionId (Subscription Active))
  }

instance Client c =>
         Client (Subscriptions c) where
  requestSubscription = requestSubscription . client
  getHubLinks = getHubLinks . client

newSubscriptions :: URI -> c -> IO (Subscriptions c)
newSubscriptions baseUri client =
  Subscriptions baseUri client <$> newMVar HM.empty <*> newMVar HM.empty

randomIdStr :: IO String
randomIdStr = return "foo"

createPending
  :: Subscriptions c
  -> SubscriptionId
  -> SubscriptionRequest
  -> ExceptT SubscribeError IO ()
createPending subscriptions subscriptionId req = do
  ready <- lift newEmptyMVar
  lift $
    modifyMVar_
      (pending subscriptions)
      (return . HM.insert subscriptionId (Pending req ready))

type ContentDistributionCallback = ContentDistribution -> IO ()

findPendingSubscription :: Subscriptions c
                        -> SubscriptionId
                        -> IO (Maybe (Subscription Pending))
findPendingSubscription subscriptions subscriptionId =
  HM.lookup subscriptionId <$> readMVar (pending subscriptions)

findActiveSubscription :: Subscriptions c
                       -> SubscriptionId
                       -> IO (Maybe (Subscription Active))
findActiveSubscription subscriptions subscriptionId =
  HM.lookup subscriptionId <$> readMVar (active subscriptions)

subscribe
  :: Client c
  => Subscriptions c
  -> Hub
  -> Topic
  -> IO (Either SubscribeError SubscriptionId)
subscribe subscriptions hub topic =
  runExceptT $ do
    idStr <- lift randomIdStr
    let base = baseUri subscriptions
        callbackUri =
          CallbackURI (base {uriPath = uriPath base ++ "/" ++ idStr})
        subscriptionId = SubscriptionId (C.pack idStr)
        subReq = SubscriptionRequest callbackUri Subscribe topic 3600
    lift $ print subReq
    -- First create a pending subscription.
    createPending subscriptions subscriptionId subReq
    -- Then actually request the subscription from the hub, as it might
    -- synchronously validate and verify the subscription.
    requestSubscription (client subscriptions) hub subReq
    return subscriptionId

awaitActiveSubscription
  :: Client c
  => Subscriptions c
  -> SubscriptionId
  -> IO (Either SubscribeError (Chan ContentDistribution))
awaitActiveSubscription subscriptions subscriptionId =
  runExceptT $ do
    Pending _ pendingResult <-
      do pending <- lift (findPendingSubscription subscriptions subscriptionId)
         maybe
           (throwError (UnexpectedError "Pending subscription not found."))
           return
           pending
    Active _ notifications <- ExceptT (readMVar pendingResult)
    return notifications

deny :: Subscriptions c -> SubscriptionId -> Denial -> IO Bool
deny subscriptions subscriptionId denial =
  findPendingSubscription subscriptions subscriptionId >>= \case
    Just (Pending _ result) ->
      putMVar result (Left (SubscriptionDenied denial)) *> return True
    Nothing -> return False

verify :: Subscriptions c -> SubscriptionId -> VerificationRequest -> IO Bool
verify subscriptions subscriptionId VerificationRequest {topic = verTopic} = do
  found <- findPendingSubscription subscriptions subscriptionId
  -- readMVar (pending subscriptions) >>= print
  -- readMVar (active subscriptions) >>= print
  case found of
    Just (Pending subReq@SubscriptionRequest {topic = subTopic} result)
      | verTopic == subTopic -> do
        notifications <- newChan
        putMVar result (Right (Active subReq notifications))
        return True
      | otherwise -> do
        putMVar result (Left VerificationFailed)
        return False
    Nothing -> return False

distributeContent :: Subscriptions c
                  -> SubscriptionId
                  -> ContentDistribution
                  -> IO Bool
distributeContent subscriptions subscriptionId notification =
  findActiveSubscription subscriptions subscriptionId >>= \case
    Just (Active _ chan) -> writeChan chan notification *> return True
    Nothing -> return False
