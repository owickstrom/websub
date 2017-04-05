{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.WebSub where

import           Data.ByteString.Lazy         (ByteString)
import           Data.Hashable
import           Network.HTTP.Media.MediaType (MediaType)
import           Network.URI                  (URI)


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


newtype Subscriber
  = Subscriber { callbackURI :: CallbackURI
               }
  deriving (Eq, Ord, Show)


data SubscriptionMode
  = Subscribe
  | Unsubscribe
  deriving (Eq, Ord, Show)

-- TODO: Add support for lease_seconds and secret

data SubscriptionRequest
  = SubscriptionRequest { subscriber :: Subscriber
                        , mode       :: SubscriptionMode
                        }
  deriving (Eq, Ord, Show)


data Notification
  = Notification { topic       :: Topic
                 , contentType :: MediaType
                 , body        :: ByteString
                 }
  deriving (Eq, Ord, Show)
