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
  , distributeContentAuthenticated
  ) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad (forever)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)

import Crypto.Hash
import Crypto.MAC.HMAC

import Data.ByteArray (Bytes, convert, constEq)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as C
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
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
  | UnexpectedError BS.ByteString
  deriving (Show, Eq, Ord)

class Client c where
  requestSubscription :: c
                      -> Hub
                      -> SubscriptionRequest CallbackURI
                      -> ExceptT SubscribeError IO ()
  getHubLinks :: c -> Topic -> IO [Hub]

type ContentDistributionCallback = ContentDistribution () -> IO ()

data Pending =
  Pending (SubscriptionRequest ContentDistributionCallback)
          (MVar (Either SubscribeError Subscription))

newtype Subscription =
  Subscription (SubscriptionRequest ContentDistributionCallback)

data Subscriptions c = Subscriptions
  { baseUri :: URI
  , client :: c
  , pending :: MVar (HashMap SubscriptionId Pending)
  , active :: MVar (HashMap SubscriptionId Subscription)
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
  -> SubscriptionRequest ContentDistributionCallback
  -> ExceptT SubscribeError IO ()
createPending subscriptions subscriptionId req = do
  ready <- lift newEmptyMVar
  lift $
    modifyMVar_
      (pending subscriptions)
      (return . HM.insert subscriptionId (Pending req ready))

activate :: Subscriptions c -> SubscriptionId -> Subscription -> IO ()
activate subscriptions subscriptionId subscription =
  modifyMVar_
    (active subscriptions)
    (return . HM.insert subscriptionId subscription)

findPendingSubscription :: Subscriptions c
                        -> SubscriptionId
                        -> IO (Maybe Pending)
findPendingSubscription subscriptions subscriptionId =
  HM.lookup subscriptionId <$> readMVar (pending subscriptions)

findActiveSubscription :: Subscriptions c
                       -> SubscriptionId
                       -> IO (Maybe Subscription)
findActiveSubscription subscriptions subscriptionId =
  HM.lookup subscriptionId <$> readMVar (active subscriptions)

subscribe
  :: Client c
  => Subscriptions c
  -> Hub
  -> SubscriptionRequest ContentDistributionCallback
  -> IO (Either SubscribeError SubscriptionId)
subscribe subscriptions hub req =
  runExceptT $ do
    idStr <- lift randomIdStr
    let base = baseUri subscriptions
        callbackUri =
          CallbackURI (base {uriPath = uriPath base ++ "/" ++ idStr})
        subscriptionId = SubscriptionId (C.pack idStr)
    -- First create a pending subscription.
    createPending subscriptions subscriptionId req
    -- Then request the subscription from the hub. The hub might
    -- synchronously validate and verify the subscription, thus the
    -- subscription has to be created and added as pending before.
    requestSubscription (client subscriptions) hub $
      req {callback = callbackUri}
    return subscriptionId

awaitActiveSubscription
  :: Client c
  => Subscriptions c -> SubscriptionId -> IO (Either SubscribeError ())
awaitActiveSubscription subscriptions subscriptionId =
  runExceptT $ do
    Pending _ pendingResult <-
      do pending <- lift (findPendingSubscription subscriptions subscriptionId)
         maybe
           (throwError (UnexpectedError "Pending subscription not found."))
           return
           pending
    Subscription req <- ExceptT (readMVar pendingResult)
    lift $ removePending subscriptions subscriptionId
    return ()

removePending :: Subscriptions c -> SubscriptionId -> IO ()
removePending subscriptions subscriptionId =
  modifyMVar_ (pending subscriptions) (return . HM.delete subscriptionId)

deny :: Subscriptions c -> SubscriptionId -> Denial -> IO Bool
deny subscriptions subscriptionId denial =
  findPendingSubscription subscriptions subscriptionId >>= \case
    Just (Pending _ result) ->
      putMVar result (Left (SubscriptionDenied denial)) *> return True
    Nothing -> return False

verify :: Subscriptions c -> SubscriptionId -> VerificationRequest -> IO Bool
verify subscriptions subscriptionId VerificationRequest {topic = verTopic} =
  findPendingSubscription subscriptions subscriptionId >>= \case
    Just (Pending subReq@SubscriptionRequest {topic = subTopic, callback} result)
      | verTopic == subTopic -> do
        let sub = Subscription subReq
        activate subscriptions subscriptionId sub
        putMVar result (Right sub)
        return True
      | otherwise -> do
        putMVar result (Left VerificationFailed)
        return False
    Nothing -> return False

distributeContent :: Subscriptions c
                  -> SubscriptionId
                  -> ContentDistribution ()
                  -> IO Bool
distributeContent subscriptions subscriptionId distribution =
  findActiveSubscription subscriptions subscriptionId >>= \case
    Just (Subscription SubscriptionRequest {callback, secret = Nothing}) ->
      callback distribution *> return True
    _ -> return False

isValidDigest :: Secret -> BS.ByteString -> ContentDigest -> Bool
isValidDigest secret body digest =
  case digest' secret digest body of
    Just expectedBytes -> Base16.encode expectedBytes `constEq` signature digest
    Nothing -> False
  where
    digest' :: Secret -> ContentDigest -> BS.ByteString -> Maybe BS.ByteString
    digest' (Secret secret) ContentDigest {method} body
      | method == "sha1" = Just (convert (hmac secret body :: HMAC SHA1))
      | method == "sha256" = Just (convert (hmac secret body :: HMAC SHA256))
      | method == "sha384" = Just (convert (hmac secret body :: HMAC SHA384))
      | method == "sha512" = Just (convert (hmac secret body :: HMAC SHA512))
      | otherwise = Nothing

distributeContentAuthenticated
  :: Subscriptions c
  -> SubscriptionId
  -> ContentDistribution ContentDigest
  -> IO Bool
distributeContentAuthenticated subscriptions subscriptionId distribution =
  findActiveSubscription subscriptions subscriptionId >>= \case
    Just (Subscription req) -> distributeReq req
    Nothing -> return False
  where
    distributeReq :: SubscriptionRequest ContentDistributionCallback -> IO Bool
    distributeReq SubscriptionRequest {callback, secret} = do
      let shouldDistribute =
            case (secret, distribution) of
              (Just secret, ContentDistribution {body, digest}) ->
                isValidDigest secret body digest
              _ -> True
      when shouldDistribute (callback distribution {digest = ()})
      return shouldDistribute
