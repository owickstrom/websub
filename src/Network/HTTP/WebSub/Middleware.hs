{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.HTTP.WebSub.Middleware
  ( subscriptionCallbacks
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as HM
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Types.Method (methodGet, methodPost)
import Network.HTTP.Types.Status (status202, status404, status400)
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
      | method == methodGet && not (BS.null subscriptionId) ->
        case fromForm (queryToForm (queryString req)) of
          Left err ->
            respond $ responseLBS status400 [] (LBS.fromStrict (encodeUtf8 err))
          Right verReq
            -- print verReq
           -> do
            verified <-
              verify subscriptions (SubscriptionId subscriptionId) verReq
            -- print verified
            if verified
              then respond $ responseLBS status202 [] (challenge verReq)
              else respond $ responseLBS status404 [] "Subscription Not Found"
      | otherwise -> app req respond
    (_, Nothing) -> app req respond
