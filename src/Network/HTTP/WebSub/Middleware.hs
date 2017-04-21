{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | A WAI middleware for integrating WebSub into Haskell web
-- applications.
module Network.HTTP.WebSub.Middleware
  ( subscriptionCallbacks
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (CI)
import qualified Data.HashMap.Lazy as HM
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Media ((//))
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (methodGet, methodPost)
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI (Query)
import Network.HTTP.WebSub
import Network.HTTP.WebSub.Subscriber
import Network.Wai
import Web.FormUrlEncoded (Form(..), fromForm)

queryToForm :: Query -> Form
queryToForm items = Form (HM.fromList (map toEntry items))
  where
    toEntry :: (BS.ByteString, Maybe BS.ByteString) -> (Text, [Text])
    toEntry =
      \case
        (k, Just v) -> (decodeUtf8 k, [decodeUtf8 v])
        (k, Nothing) -> (decodeUtf8 k, [])

getHeader :: CI BS.ByteString -> Request -> Maybe BS.ByteString
getHeader name = lookup name . requestHeaders

getSignature :: Request -> Maybe ContentDigest
getSignature req = do
  x <- getHeader "X-Hub-Signature" req
  case C.split '=' x of
    [method, signature] -> Just ContentDigest {method, signature}
    _ -> Nothing

verifySubscriptionHandler :: Subscriptions c -> SubscriptionId -> Application
verifySubscriptionHandler subscriptions subscriptionId req respond =
  case fromForm (queryToForm (queryString req)) of
    Left err ->
      respond $ responseLBS status400 [] (LBS.fromStrict (encodeUtf8 err))
    Right verReq -> do
      verified <- verify subscriptions subscriptionId verReq
      if verified
        then respond $
             responseLBS status202 [] (LBS.fromStrict (challenge verReq))
        else respond $ responseLBS status404 [] "Subscription Not Found"

distributeContentHandler :: Subscriptions c -> SubscriptionId -> Application
distributeContentHandler subscriptions subscriptionId req respond =
  case C.split '/' <$> getHeader hContentType req of
    Just [a, b] -> do
      body <- LBS.toStrict <$> strictRequestBody req
      distributed <-
        distributeWithDigest
          ContentDistribution {contentType = a // b, body = body, digest = ()}
          (getSignature req)
      if distributed
        then respond $ responseLBS status200 [] ""
        else respond $ responseLBS status400 [] "Content not distributed."
    Just parts ->
      respond $
      responseLBS
        status400
        []
        ("Could not parse content-type header: " <>
         LBS.fromStrict (C.intercalate "/" parts))
    Nothing -> respond $ responseLBS status400 [] "No content-type header set."
  where
    distributeWithDigest :: ContentDistribution ()
                         -> Maybe ContentDigest
                         -> IO Bool
    distributeWithDigest distribution =
      \case
        Just digest ->
          distributeContentAuthenticated
            subscriptions
            subscriptionId
            distribution {digest = digest}
        Nothing -> distributeContent subscriptions subscriptionId distribution

-- | Given a 'Subscriptions', and a subscription callback base URI,
-- return a middleware that receives callbacks and distributes content
-- to the correct subscribers.
subscriptionCallbacks :: Subscriptions c -> BS.ByteString -> Middleware
subscriptionCallbacks subscriptions basePath app req respond =
  case (requestMethod req, BS.stripPrefix (basePath <> "/") (rawPathInfo req)) of
    (method, Just subscriptionId)
      | BS.null subscriptionId ->
        respond $ responseLBS status404 [] "Subscription Not Found"
      | otherwise ->
        case method of
          _
            | method == methodGet ->
              verifySubscriptionHandler
                subscriptions
                (SubscriptionId subscriptionId)
                req
                respond
            | method == methodPost ->
              distributeContentHandler
                subscriptions
                (SubscriptionId subscriptionId)
                req
                respond
            | otherwise -> app req respond
    (_, Nothing) -> app req respond
