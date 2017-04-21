{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | An HTTP client for the 'Client' type class, that can be used
-- together with the "Subscriber" module.
module Network.HTTP.WebSub.HTTPClient (HTTPSubscriberClient(..)) where

import Control.Monad.Except
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.Maybe (catMaybes)

import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types
       (Link(..), LinkParam(..), linkParams)
import Network.HTTP.Simple as HTTP
import Network.HTTP.Types.Status (status202)
import Network.HTTP.WebSub
import Network.HTTP.WebSub.Subscriber
import Network.URI
       (URI, uriToString)

import Web.FormUrlEncoded

requestFromUri :: URI -> Maybe Request
requestFromUri uri
  -- Hacky way of going from URI to Request, by rendering as String and
  -- parsing it as a Request.
 = parseRequest (uriToString id uri "")

data HTTPSubscriberClient =
  HTTPSubscriberClient

instance Client HTTPSubscriberClient where
  requestSubscription _ hub subReq =
    case makeHubRequest hub of
      Just req -> do
        res <- httpLBS req
        unless (getResponseStatus res == status202) $
          throwError (UnexpectedError (LBS.toStrict (getResponseBody res)))
      Nothing -> throwError (InvalidHub hub)
    where
      makeHubRequest (Hub hub') = setupReq <$> requestFromUri hub'
        where
          setupReq =
            setRequestMethod "POST" .
            setRequestHeader
              "Content-Type"
              ["application/x-www-form-urlencoded"] .
            setRequestBodyLBS (urlEncodeAsForm subReq)
  getHubLinks _ (Topic uri) =
    case setRequestMethod "HEAD" <$> requestFromUri uri of
      Just req -> do
        res <- httpNoBody req
        return (hubLinks (getResponseHeader "Link" res))
      Nothing -> return []
    where
      isHubLink link = lookup Rel (linkParams link) == Just "hub"
      hubLinks :: [C.ByteString] -> [Hub]
      hubLinks headers =
        headers & map parseLinkHeaderBS & catMaybes & concat & filter isHubLink &
        map toHub
      toHub (Link uri' _) = Hub uri'
