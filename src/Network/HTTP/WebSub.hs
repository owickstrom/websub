{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.WebSub where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy as LBS
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Media.MediaType (MediaType)
import Network.URI (URI, parseURI)
import Web.FormUrlEncoded
       (ToForm, FromForm, toForm, fromForm, parseUnique, parseMaybe)
import Web.HttpApiData (FromHttpApiData, parseQueryParam)

newtype Topic =
  Topic URI
  deriving (Eq, Ord, Show)

instance FromHttpApiData Topic where
  parseQueryParam t =
    case parseURI (Text.unpack t) of
      Just uri -> Right (Topic uri)
      Nothing -> Left ("Invalid Topic URI: " <> t)

newtype Hub =
  Hub URI
  deriving (Eq, Ord, Show)

newtype CallbackURI =
  CallbackURI URI
  deriving (Eq, Ord, Show)

data SubscriptionMode
  = Subscribe
  | Unsubscribe
  deriving (Eq, Ord, Show)

instance FromHttpApiData SubscriptionMode where
  parseQueryParam t
    | t == "subscribe" = Right Subscribe
    | t == "unsubscribe" = Right Unsubscribe
    | otherwise = Left ("Invalid SubscriptionMode: " <> t)

data SubscriptionRequest = SubscriptionRequest
  { callback :: CallbackURI
  , mode :: SubscriptionMode
  , topic :: Topic
  , leaseSeconds :: Int
                        -- TODO: Add support for secret
  } deriving (Eq, Ord, Show)

instance ToForm SubscriptionRequest where
  toForm SubscriptionRequest { callback = CallbackURI callback
                             , mode = mode
                             , topic = Topic topic
                             } =
    [ ("hub.callback", tshow callback)
    , ("hub.mode", modeToValue mode)
    , ("hub.topic", tshow topic)
    ]
    where
      tshow = Text.pack . show
      modeToValue =
        \case
          Subscribe -> "subscribe"
          Unsubscribe -> "unsubscribe"

data Denial = Denial
  { topic :: Topic
  , reason :: ByteString
  } deriving (Show, Eq, Ord)

data VerificationRequest = VerificationRequest
  { mode :: SubscriptionMode
  , topic :: Topic
  , challenge :: ByteString
  , leaseSeconds :: Maybe Int
  } deriving (Eq, Ord, Show)

instance FromForm VerificationRequest where
  fromForm f =
    VerificationRequest <$> parseUnique "hub.mode" f <*>
    parseUnique "hub.topic" f <*>
    parseChallenge <*>
    parseMaybe "hub.lease_seconds" f
    where
      parseChallenge =
        LBS.fromStrict . encodeUtf8 <$> parseUnique "hub.challenge" f

data ContentDistribution = ContentDistribution
  { contentType :: MediaType
  , body :: ByteString
  } deriving (Eq, Ord, Show)
