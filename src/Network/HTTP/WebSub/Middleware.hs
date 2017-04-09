{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Network.HTTP.WebSub.Middleware
       ( subscriptionCallbacks
       ) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.HashMap.Lazy as HM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Network.HTTP.WebSub
import Network.HTTP.WebSub.Subscriber
import Network.HTTP.Types.Status (status202, status404)
import Network.HTTP.Types.Method (methodGet, methodPost)
import Network.Wai
import Web.FormUrlEncoded (Form(..),fromForm)
import Network.HTTP.Types.URI (Query)

queryToForm :: Query -> Form
queryToForm items =
  Form (HM.fromList (map toEntry items))
  where
    toEntry :: (BS.ByteString, Maybe BS.ByteString) -> (Text, [Text])
    toEntry =
      \case
        (k, Just v) -> (decodeUtf8 k, [decodeUtf8 v])
        (k, Nothing) -> (decodeUtf8 k, [])

subscriptionCallbacks :: Subscriptions c -> Middleware
subscriptionCallbacks subscriptions app req respond =
  -- TODO: extract subscription id for pending subscription
  case (requestMethod req, BS.stripPrefix "/subscriptions/" (rawPathInfo req)) of
    (method, Just subscriptionId)
      | method == methodGet && not (BS.null subscriptionId) ->
        case fromForm (queryToForm (queryString req)) of
          Left err -> respond $ responseLBS status404 [] ""
          Right verReq -> respond $ responseLBS status202 [] (challenge verReq)
      | otherwise -> app req respond
    (_, Nothing) -> app req respond
