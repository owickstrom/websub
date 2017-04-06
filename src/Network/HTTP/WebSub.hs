{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
module Network.HTTP.WebSub where

import           Data.ByteString.Lazy         (ByteString)
import           Data.Hashable
import qualified Data.Text                    as Text
import           Network.HTTP.Media.MediaType (MediaType)
import           Network.URI                  (URI)
import           Web.FormUrlEncoded


newtype Topic
  = Topic URI
  deriving (Eq, Ord, Show)


newtype Hub
  = Hub URI
  deriving (Eq, Ord, Show)


newtype CallbackURI
  = CallbackURI URI
  deriving (Eq, Ord, Show)


instance Hashable CallbackURI where
  hashWithSalt salt =
    hashWithSalt salt . show


data SubscriptionMode
  = Subscribe
  | Unsubscribe
  deriving (Eq, Ord, Show)


data SubscriptionRequest
  = SubscriptionRequest { callback :: CallbackURI
                        , mode     :: SubscriptionMode
                        , topic    :: Topic
                        -- TODO: Add support for lease_seconds and secret
                        }
  deriving (Eq, Ord, Show)

instance ToForm SubscriptionRequest where
  toForm SubscriptionRequest { callback = CallbackURI callback, mode = mode, topic = Topic topic} =
    [ ("callback", tshow callback)
    , ("mode", modeToValue mode)
    , ("topic", tshow topic)
    ]
    where
      tshow = Text.pack . show
      modeToValue =
        \case
          Subscribe -> "subscribe"
          Unsubscribe -> "unsubscribe"


data Notification
  = Notification { hub         :: Hub
                 , topic       :: Topic
                 , contentType :: MediaType
                 , body        :: ByteString
                 }
  deriving (Eq, Ord, Show)
