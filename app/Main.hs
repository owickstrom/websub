{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Monad (void, forever)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Maybe
import Data.Monoid ((<>))
import Network.HTTP.Media.MediaType (MediaType, (//))
import Network.HTTP.Types.Status (status404)
import Network.HTTP.WebSub
import Network.HTTP.WebSub.HTTPClient
import Network.HTTP.WebSub.Middleware
import Network.HTTP.WebSub.Subscriber
import Network.URI
import Network.Wai
import Network.Wai.Handler.Warp (run)

import System.Environment
import System.IO (hPutStrLn, stderr)
import Text.Printf

usage :: IO ()
usage = hPutStrLn stderr "Usage: websub-exe BASE_URI TOPIC_URI"

main :: IO ()
main =
  getArgs >>= \case
    [baseUri, topicUri] -> do
      base <- parseUriOrFail baseUri
      let subscriptionBasePath = "/subscriptions"
          callbackBase = base {uriPath = subscriptionBasePath}
      topic <- Topic <$> parseUriOrFail topicUri
      subscriptions <- newSubscriptions callbackBase HTTPSubscriberClient
      void $
        forkIO $ do
          threadDelay 1000000
          putStrLn "Subscribing..."
          subscribeTo topic subscriptions
      putStrLn "Starting server at http://localhost:3000"
      run
        3000
        (subscriptionCallbacks
           subscriptions
           (C.pack subscriptionBasePath)
           notFound)
    args -> usage
  where
    parseUriOrFail s =
      fromMaybe (fail (printf "Invalid URI: %s" s)) (return <$> parseURI s)
    subscribeTo topic subscriptions =
      getHubLinks subscriptions topic >>= \case
        [] -> putStrLn "No hub found."
        hub:_ -> do
          printf "Subscribing to %s through %s.\n" (show topic) (show hub)
          res <-
            subscribe
              subscriptions
              hub
              SubscriptionRequest
              { topic = topic
              , mode = Subscribe
              , secret = Nothing
              , leaseSeconds = 3600
              , callback = onContentDistribution
              }
          case res of
            Left err -> printf "Subscription failed: %s\n" (show err)
            Right subscriptionId -> do
              putStrLn $ "Subscription ID: " ++ show subscriptionId
              awaitActiveSubscription subscriptions subscriptionId >>= \case
                Left err -> printf "Subscription failed: %s\n" (show err)
                Right () ->
                  printf "Subscription %s active.\n" (show subscriptionId)
    onContentDistribution ContentDistribution {contentType, body} = do
      printf "Content Type: %s\n" (show contentType)
      LC.putStrLn body
    notFound req respond = respond $ responseLBS status404 [] "Not Found"
