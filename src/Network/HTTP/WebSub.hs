{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
=WebSub

This package is an implementation of the
<https://www.w3.org/TR/websub/ WebSub> working draft specification. It
lets you subscribe to topics and receive published notifications in
any WAI-based web application.

A more extensive documentation, and examples of usage, can be found at
<https://owickstrom.github.io/websub/>. For a general overview of the
purpose of WebSub, see the specification at
<https://www.w3.org/TR/websub/>.
-}
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

-- | An HTTP resource URI that one can subscribe to.
newtype Topic =
  Topic URI
  deriving (Eq, Ord, Show)

instance FromHttpApiData Topic where
  parseQueryParam t =
    case parseURI (Text.unpack t) of
      Just uri -> Right (Topic uri)
      Nothing -> Left ("Invalid Topic URI: " <> t)

-- | A server HTTP resource URI to a hub.
newtype Hub =
  Hub URI
  deriving (Eq, Ord, Show)

-- | The callback URI to which hubs post notifications.
newtype CallbackURI =
  CallbackURI URI
  deriving (Eq, Ord, Show)

instance ToForm CallbackURI where
  toForm (CallbackURI uri) = [("hub.callback", Text.pack (show uri))]

-- | The two WebSub subscription modes - whether one wants to
-- subscribe to, or unsubscribe from, a topic.
data SubscriptionMode
  = Subscribe
  | Unsubscribe
  deriving (Eq, Ord, Show)

instance FromHttpApiData SubscriptionMode where
  parseQueryParam t
    | t == "subscribe" = Right Subscribe
    | t == "unsubscribe" = Right Unsubscribe
    | otherwise = Left ("Invalid SubscriptionMode: " <> t)

-- | A secret string passed to the hub to perform Authenticated
-- Content Distribution, as described at
-- <https://www.w3.org/TR/2016/WD-websub-20161124/#authenticated-content-distribution>.
newtype Secret =
  Secret ByteString
  deriving (Eq)

instance Show Secret where
  show _ = "Secret (...)"

-- | The subscription request, or unsubscription request, sent to hubs.
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

-- | For what topic, and for what reason, a subscription was denied.
data Denial = Denial
  { topic :: Topic
  , reason :: ByteString
  } deriving (Show, Eq, Ord)

-- | The verification request from a hub to the subscriber to verify
-- the subscription.
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

-- | The digest value sent by the hub, based on the secret given in
-- the subscription request, together with the used algorithm/method.
data ContentDigest = ContentDigest
  { method :: ByteString
  , signature :: ByteString
  } deriving (Eq, Ord, Show)

-- | A content distribution is the data sent by the hub when notifying
-- a subscriber about a topic update.
data ContentDistribution digest = ContentDistribution
  { contentType :: MediaType
  , body :: ByteString
  -- | The cryptographic digest of the content. If using a 'Secret',
  -- and thus Authenticated Content Distribution, the 'digest'
  -- parameter will be a 'ContentDigest' value, otherwise it will be
  -- '()'.
  , digest :: digest
  } deriving (Eq, Ord, Show)
