{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Network.HTTP.WebSub.SubscriberSpec where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Except
import Network.HTTP.Media ((//))
import Network.HTTP.WebSub
import Network.HTTP.WebSub.Subscriber
import Network.URI as URI

import Test.Hspec

data StubClient = StubClient
  { requestSubscriptionResponse :: Either SubscribeError ()
  , links :: [Hub]
  }

instance Client StubClient where
  requestSubscription client _ _ =
    ExceptT (return (requestSubscriptionResponse client))
  getHubLinks client _ = return (links client)

shouldFailWith
  :: (Eq e, Show e)
  => IO (Either e a) -> e -> Expectation
shouldFailWith res expectedErr =
  res >>= \case
    Left actualErr -> actualErr `shouldBe` expectedErr
    Right _ -> expectationFailure "Did not expected success."

noop :: ContentDistributionCallback
noop _ = return ()

spec :: Spec
spec = do
  let topic = Topic URI.nullURI
      hub = Hub URI.nullURI
      callbackUri = CallbackURI URI.nullURI
      contentDistribution = ContentDistribution ("text" // "plain") "Hello!"
  describe "subscribe" $ do
    it "fails when client fails" $ do
      let err = UnexpectedError "oops"
          client = StubClient (Left err) []
      subs <- newSubscriptions URI.nullURI client
      subscribe subs hub topic (const (return ())) `shouldFailWith` err
    it "fails when hub denies the subscription" $ do
      let client = StubClient (Right ()) []
          denial = Denial topic "No, sir, you cannot have it."
      subs <- newSubscriptions URI.nullURI client
      subscribe subs hub topic noop >>= \case
        Left err -> expectationFailure (show err)
        Right sId -> do
          deny subs sId denial `shouldReturn` True
          awaitActiveSubscription subs sId `shouldFailWith`
            SubscriptionDenied denial
    it "does not activate the subscription when verified with incorrect values" $ do
      let client = StubClient (Right ()) []
          verReq =
            VerificationRequest
              Subscribe
              (Topic URI.nullURI {uriPath = "/foo/bar"})
              ""
              Nothing
      subs <- newSubscriptions URI.nullURI client
      subscribe subs hub topic noop >>= \case
        Left err -> expectationFailure (show err)
        Right sId -> do
          verify subs sId verReq `shouldReturn` False
          awaitActiveSubscription subs sId `shouldFailWith`
            VerificationFailed
    it "activates the subscription when verified with correct values" $ do
      let client = StubClient (Right ()) []
          verReq = VerificationRequest Subscribe topic "" Nothing
      subs <- newSubscriptions URI.nullURI client
      subscribe subs hub topic noop >>= \case
        Left err -> expectationFailure (show err)
        Right sId -> do
          verify subs sId verReq `shouldReturn` True
          void <$>
            awaitActiveSubscription subs sId `shouldReturn` Right ()
    it "distributes content" $ do
      let client = StubClient (Right ()) []
          verReq = VerificationRequest Subscribe topic "" Nothing
      distributed <- newEmptyMVar
      subs <- newSubscriptions URI.nullURI client
      let onContentDistribution = putMVar distributed
      subscribe subs hub topic onContentDistribution >>= \case
        Left err -> expectationFailure (show err)
        Right sId -> do
          verify subs sId verReq `shouldReturn` True
          void <$>
            awaitActiveSubscription subs sId `shouldReturn` Right ()
          distributeContent subs sId contentDistribution `shouldReturn` True
      readMVar distributed `shouldReturn` contentDistribution
