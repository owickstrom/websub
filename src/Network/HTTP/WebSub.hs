{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.WebSub where

import           Data.ByteString.Lazy         (ByteString)
import           Network.HTTP.Media.MediaType (MediaType)
import           Network.URI                  (URI)


newtype Topic = Topic URI


newtype Hub = Hub URI


newtype Subscriber
  = Subscriber { callbackURI :: URI
               }


data SubscriptionMode
  = Subscribe
  | Unsubscribe


-- TODO: Add support for lease_seconds and secret

data SubscriptionRequest
  = SubscriptionRequest { subscriber :: Subscriber
                        , mode       :: SubscriptionMode
                        }


data Notification
  = Notification { contentType :: MediaType
                 , body        :: ByteString
                 }
