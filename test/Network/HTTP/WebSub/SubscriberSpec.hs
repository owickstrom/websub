{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Network.HTTP.WebSub.SubscriberSpec where

import Network.HTTP.WebSub
import Network.HTTP.WebSub.Subscriber
import Network.URI as URI

import Test.Hspec

data StubClient = StubClient
  { requestSubscriptionResponse :: Either SubscribeError ()
  , links :: [Hub]
  }

instance Client StubClient where
  requestSubscription client _ _ = return (requestSubscriptionResponse client)
  getHubLinks client _ = return (links client)

spec :: Spec
spec = do
  let topic = Topic URI.nullURI
      hub = Hub URI.nullURI

  describe "subscribe" $ do

    it "fails when client fails" $ do
      let err = UnexpectedError "oops"
          client = StubClient (Left err) []
      subs <- newSubscriptions URI.nullURI client
      subscribe subs hub topic >>=
        \case
          Left actualErr -> actualErr `shouldBe` err
          Right _ -> expectationFailure "Did not expected subscribe to succeed."
