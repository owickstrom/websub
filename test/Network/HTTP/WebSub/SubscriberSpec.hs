{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Network.HTTP.WebSub.SubscriberSpec where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
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

shouldFailWith :: (Eq e, Show e) => IO (Either e a) -> e -> Expectation
shouldFailWith res expectedErr =
  res >>=
  \case
    Left actualErr -> actualErr `shouldBe` expectedErr
    Right _ -> expectationFailure "Did not expected success."

spec :: Spec
spec = do
  let topic = Topic URI.nullURI
      hub = Hub URI.nullURI
      callbackUri = CallbackURI URI.nullURI

  describe "subscribe" $ do

    it "fails when client fails" $ do
      let err = UnexpectedError "oops"
          client = StubClient (Left err) []
      subs <- newSubscriptions URI.nullURI client
      subscribe subs hub topic `shouldFailWith` err

    it "fails when hub denies the subscription" $ do
      let client = StubClient (Right ()) []
          denial = Denial topic "No, sir, you cannot have it."
      subs <- newSubscriptions URI.nullURI client
      subscribe subs hub topic
      deny subs callbackUri denial `shouldReturn` True
      awaitActiveSubscription subs callbackUri `shouldFailWith` SubscriptionDenied denial
