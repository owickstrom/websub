{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.HTTP.WebSub.Middleware
  ( subscriptionCallbacks
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types
       (Link(..), LinkParam(..), linkParams, href)
import Network.HTTP.Media ((//))
import Network.HTTP.Types.Header
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

subscriptionCallbacks :: Subscriptions c -> BS.ByteString -> Middleware
subscriptionCallbacks subscriptions basePath app req respond =
  case (requestMethod req, BS.stripPrefix (basePath <> "/") (rawPathInfo req)) of
    (method, Just subscriptionId)
      | BS.null subscriptionId ->
        respond $ responseLBS status404 [] "Subscription Not Found"
      | otherwise ->
        case method of
          method
            | method == methodGet ->
              case fromForm (queryToForm (queryString req)) of
                Left err ->
                  respond $
                  responseLBS status400 [] (LBS.fromStrict (encodeUtf8 err))
                Right verReq -> do
                  verified <-
                    verify subscriptions (SubscriptionId subscriptionId) verReq
                  if verified
                    then respond $ responseLBS status202 [] (challenge verReq)
                    else respond $
                         responseLBS status404 [] "Subscription Not Found"
            | method == methodPost ->
              case C.split '/' <$> lookup hContentType (requestHeaders req) of
                Just [a, b] -> do
                  body <- lazyRequestBody req
                  distributed <-
                    distributeContent
                      subscriptions
                      (SubscriptionId subscriptionId)
                      (ContentDistribution (a // b) body)
                  if distributed
                    then respond $ responseLBS status200 [] ""
                    else respond $
                         responseLBS status400 [] "Content not distributed."
                Just parts ->
                  respond $
                  responseLBS
                    status400
                    []
                    ("Could not parse content-type header: " <>
                     LBS.fromStrict (C.intercalate "/" parts))
                Nothing ->
                  respond $
                  responseLBS status400 [] "No content-type header set."
            | otherwise -> app req respond
    (_, Nothing) -> app req respond
