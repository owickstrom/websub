{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Network.HTTP.WebSub.SubscriberSpec where

import qualified Data.ByteString.Base16 as Base16
import Data.ByteArray (convert)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Except
import Crypto.Hash
import Crypto.MAC.HMAC
import Data.ByteString
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

testRequest :: SubscriptionRequest ContentDistributionCallback
testRequest =
  SubscriptionRequest
  { mode = Subscribe
  , topic = Topic URI.nullURI
  , leaseSeconds = 60
  , secret = Nothing
  , callback = const (return ())
  }

contentDistribution :: ContentDistribution ()
contentDistribution =
  ContentDistribution
  {contentType = "text" // "plain", body = "Hello!", digest = ()}

testDigest :: ContentDigest
testDigest = ContentDigest "sha1" digest
  where
    key, message :: ByteString
    key = "foo"
    message = "Hello!"
    digest = Base16.encode (convert (hmacGetDigest (hmac key message) :: Digest SHA1))

incorrectDigest :: ContentDigest
incorrectDigest = ContentDigest "" ""

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
      subscribe subs hub testRequest `shouldFailWith` err
    it "fails when hub denies the subscription" $ do
      let client = StubClient (Right ()) []
          denial = Denial topic "No, sir, you cannot have it."
      subs <- newSubscriptions URI.nullURI client
      subscribe subs hub testRequest >>= \case
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
      subscribe subs hub testRequest >>= \case
        Left err -> expectationFailure (show err)
        Right sId -> do
          verify subs sId verReq `shouldReturn` False
          awaitActiveSubscription subs sId `shouldFailWith` VerificationFailed
    it "activates the subscription when verified with correct values" $ do
      let client = StubClient (Right ()) []
          verReq = VerificationRequest Subscribe topic "" Nothing
      subs <- newSubscriptions URI.nullURI client
      subscribe subs hub testRequest >>= \case
        Left err -> expectationFailure (show err)
        Right sId -> do
          verify subs sId verReq `shouldReturn` True
          void <$> awaitActiveSubscription subs sId `shouldReturn` Right ()
    it "distributes content" $ do
      distributed <- newEmptyMVar
      let client = StubClient (Right ()) []
          verReq = VerificationRequest Subscribe topic "" Nothing
      subs <- newSubscriptions URI.nullURI client
      subscribe subs hub testRequest {callback = putMVar distributed} >>= \case
        Left err -> expectationFailure (show err)
        Right sId -> do
          verify subs sId verReq `shouldReturn` True
          void <$> awaitActiveSubscription subs sId `shouldReturn` Right ()
          distributeContent subs sId contentDistribution `shouldReturn` True
      tryReadMVar distributed `shouldReturn`
        Just contentDistribution {digest = ()}
    it "distributes content with authentication" $ do
      distributed <- newEmptyMVar
      let client = StubClient (Right ()) []
          subReq =
            testRequest
            {callback = putMVar distributed, secret = Just (Secret "foo")}
          verReq = VerificationRequest Subscribe topic "" Nothing
          authDistribution = contentDistribution {digest = testDigest}
      subs <- newSubscriptions URI.nullURI client
      subscribe subs hub subReq >>= \case
        Left err -> expectationFailure (show err)
        Right sId -> do
          verify subs sId verReq `shouldReturn` True
          void <$> awaitActiveSubscription subs sId `shouldReturn` Right ()
          distributeContentAuthenticated subs sId authDistribution `shouldReturn` True
      tryReadMVar distributed `shouldReturn` Just contentDistribution
    it "does not distribute content when authentication does not match" $ do
      let client = StubClient (Right ()) []
          subReq = testRequest {secret = Just (Secret "foo")}
          verReq = VerificationRequest Subscribe topic "" Nothing
          incorrectDistribution = contentDistribution {digest = incorrectDigest}
      subs <- newSubscriptions URI.nullURI client
      subscribe subs hub subReq >>= \case
        Left err -> expectationFailure (show err)
        Right sId -> do
          verify subs sId verReq `shouldReturn` True
          void <$> awaitActiveSubscription subs sId `shouldReturn` Right ()
          distributeContentAuthenticated subs sId incorrectDistribution `shouldReturn` False
