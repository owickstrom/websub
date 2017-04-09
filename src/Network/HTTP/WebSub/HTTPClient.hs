{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.HTTP.WebSub.HTTPClient where

import Control.Monad.Except
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types
       (Link(..), LinkParam(..), linkParams)
import Network.HTTP.Media.MediaType (MediaType)
import Network.HTTP.Simple as HTTP
import Network.HTTP.Types.Status (status202)
import Network.HTTP.WebSub
import Network.HTTP.WebSub.Subscriber
import Network.URI
       (URI, uriAuthority, uriRegName, uriScheme, uriToString)

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
    case makeHubRequest hub subReq of
      Just req -> do
        res <- httpLBS req
        lift $ print res
        unless (getResponseStatus res == status202) $
          throwError (UnexpectedError (getResponseBody res))
      Nothing -> throwError (InvalidHub hub)
    where
      makeHubRequest (Hub hub) subReq =
        requestFromUri hub
        & fmap (setRequestMethod "POST")
        & fmap (setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"])
        & fmap (setRequestBodyLBS (urlEncodeAsForm subReq))
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
      toHub (Link uri _) = Hub uri
