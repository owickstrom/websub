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
  , SubscribeResult(..)
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

import Control.Concurrent.MVar
import Control.Monad.Except

import Crypto.Hash
import Crypto.MAC.HMAC

import Data.ByteArray (convert, constEq)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as C
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Maybe (fromMaybe, maybe)
import Data.Time

import System.Random

import Network.HTTP.WebSub
import Network.URI

-- | A unique ID for a subscription.
newtype SubscriptionId =
  SubscriptionId BS.ByteString
  deriving (Show, Eq)

instance Hashable SubscriptionId where
  hashWithSalt salt = hashWithSalt salt . show

-- | The different types of errors that can occur when subscribing to
-- a topic.
data SubscribeError
  = InvalidHub Hub
  | SubscriptionDenied Denial
  | VerificationFailed
  | UnexpectedError BS.ByteString
  deriving (Show, Eq, Ord)

-- | The result of subscribing to a topic successfully, including
-- when, and in how many seconds, the subscription expires. This value
-- is ultimately decided by the hub, as per the WebSub specification.
data SubscribeResult = SubscribeResult
  { topic :: Topic
  , expires :: UTCTime
  , leaseSeconds :: Int
  } deriving (Show, Eq, Ord)

-- | A Client sends subscription requests to hubs, and extracts valid
-- 'Hub' values from 'Topic' HTTP resource headers.
class Client c where
  requestSubscription :: c
                      -> Hub
                      -> SubscriptionRequest CallbackURI
                      -> ExceptT SubscribeError IO ()
  getHubLinks :: c -> Topic -> IO [Hub]

-- | A callback function, returning an 'IO' action, is evaluated when
-- the content is distributed for the corresponding subscription.
type ContentDistributionCallback = ContentDistribution () -> IO ()

-- | A pending subscription, i.e. on that has not been denied or
-- verified yet.
data Pending =
  Pending (SubscriptionRequest ContentDistributionCallback)
          (MVar (Either SubscribeError Subscription))

-- | A subscription that has been verified, and that is considered
-- active.
data Subscription =
  Subscription (SubscriptionRequest ContentDistributionCallback)
               SubscribeResult

-- | The 'Subscriptions' structure holds all data needed to request
-- new subscriptions, and to distribute content for existing
-- subscriptions. This data structure is the center of the
-- "Subscriptions" module.
data Subscriptions c = Subscriptions
  { baseUri :: URI
  , client :: c
  , pending :: MVar (HashMap SubscriptionId Pending)
  , active :: MVar (HashMap SubscriptionId Subscription)
  }

-- | A 'Subscriptions' value is itself a 'Client', by proxying the
-- underlying 'Client'.
instance Client c =>
         Client (Subscriptions c) where
  requestSubscription = requestSubscription . client
  getHubLinks = getHubLinks . client

-- | Create a 'Subscriptions' value using a base 'URI' and a 'Client'.
newSubscriptions :: URI -> c -> IO (Subscriptions c)
newSubscriptions baseUri client =
  Subscriptions baseUri client <$> newMVar HM.empty <*> newMVar HM.empty

-- Generates a new random subscription ID string.
randomIdStr :: IO String
randomIdStr = do
  gen <- newStdGen
  -- TODO: Improve this?
  return (take 32 (randomRs ('a','z') gen))

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
  => Subscriptions c
  -> SubscriptionId
  -> IO (Either SubscribeError SubscribeResult)
awaitActiveSubscription subscriptions subscriptionId =
  runExceptT $ do
    Pending _ pendingResult <-
      do pending <- lift (findPendingSubscription subscriptions subscriptionId)
         maybe
           (throwError (UnexpectedError "Pending subscription not found."))
           return
           pending
    Subscription req res <- ExceptT (readMVar pendingResult)
    lift $ removePending subscriptions subscriptionId
    return res

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
verify subscriptions subscriptionId verReq@VerificationRequest { topic = verTopic
                                                               , leaseSeconds = verLeaseSeconds
                                                               } =
  findPendingSubscription subscriptions subscriptionId >>= \case
    Just (Pending subReq@SubscriptionRequest { topic = subTopic
                                             , leaseSeconds = subLeaseSeconds
                                             , callback
                                             } result)
      | verTopic == subTopic -> do
        now <- getCurrentTime
        let seconds = fromMaybe subLeaseSeconds verLeaseSeconds
            expires = fromIntegral seconds `addUTCTime` now
            sub =
              Subscription
                subReq
                SubscribeResult
                {topic = verTopic, expires = expires, leaseSeconds = seconds}
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
    Just (Subscription SubscriptionRequest {callback, secret = Nothing} _) ->
      callback distribution *> return True
    _ -> return False

isValidDigest :: Secret -> BS.ByteString -> ContentDigest -> Bool
isValidDigest secret body digest =
  case (digest' secret digest body, Base16.decode (signature digest)) of
    (Just expected, (fromHub, rest))
      | BS.null rest -> expected `constEq` fromHub
    _ -> False
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
    Just (Subscription req _) -> distributeReq req
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
