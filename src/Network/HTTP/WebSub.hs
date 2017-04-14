{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.WebSub where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
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

instance ToForm CallbackURI where
  toForm (CallbackURI uri) = [("hub.callback", Text.pack (show uri))]

data SubscriptionMode
  = Subscribe
  | Unsubscribe
  deriving (Eq, Ord, Show)

instance FromHttpApiData SubscriptionMode where
  parseQueryParam t
    | t == "subscribe" = Right Subscribe
    | t == "unsubscribe" = Right Unsubscribe
    | otherwise = Left ("Invalid SubscriptionMode: " <> t)

newtype Secret =
  Secret ByteString
  deriving (Eq)

instance Show Secret where
  show _ = "Secret (...)"

data SubscriptionRequest cb = SubscriptionRequest
  { callback :: cb
  , mode :: SubscriptionMode
  , topic :: Topic
  , leaseSeconds :: Int
  , secret :: Maybe Secret
  } deriving (Eq, Show)

instance ToForm cb =>
         ToForm (SubscriptionRequest cb) where
  toForm SubscriptionRequest {callback, mode, topic = Topic topic, secret} =
    toForm callback <> secretField <>
    [("hub.mode", modeToValue mode), ("hub.topic", tshow topic)]
    where
      tshow = Text.pack . show
      modeToValue =
        \case
          Subscribe -> "subscribe"
          Unsubscribe -> "unsubscribe"
      secretField =
        case secret of
          Just (Secret s) -> [("hub.secret", decodeUtf8 s)]
          Nothing -> []

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
      parseChallenge = encodeUtf8 <$> parseUnique "hub.challenge" f

data ContentDigest = ContentDigest
  { method :: ByteString
  , signature :: ByteString
  } deriving (Eq, Ord, Show)

data ContentDistribution digest = ContentDistribution
  { contentType :: MediaType
  , body :: ByteString
  , digest :: digest
  } deriving (Eq, Ord, Show)
