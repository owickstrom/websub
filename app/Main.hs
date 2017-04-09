{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void, forever)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe
import Data.Monoid ((<>))
import Network.HTTP.Media.MediaType (MediaType, (//))
import Network.HTTP.Types.Status (status404)
import Network.HTTP.WebSub
import Network.HTTP.WebSub.HTTPClient
import Network.HTTP.WebSub.Subscriber
import Network.HTTP.WebSub.Middleware
import Network.URI
import Network.Wai
import Network.Wai.Handler.Warp (run)

import System.Environment
import System.IO (hPutStrLn, stderr)
import Text.Printf

usage :: IO ()
usage = hPutStrLn stderr "Usage: websub-exe TOPIC_URI"

main :: IO ()
main =
  getArgs >>= \case
    [topicUri] -> do
      topic <- Topic <$> parseUriOrFail topicUri
      subscriptions <- newSubscriptions clientBaseUri HTTPSubscriberClient
      void $ forkIO $ do
        threadDelay 1000000
        putStrLn "Subscribing..."
        subscribeTo topic subscriptions
      putStrLn "Starting server at http://localhost:3000"
      run 3000 (subscriptionCallbacks subscriptions notFound)

    args -> usage
  where
    clientBaseUri =
      URI "https:" (Just (URIAuth "" "owiwebsub.localtunnel.me" ":3000")) "/subscriptions" "" ""

    hub (Topic topicUri)
      -- Hack for now, pleases websub.rocks URI scheme.
     = Hub $ topicUri {uriPath = uriPath topicUri ++ "/hub"}

    callbackUri = CallbackURI clientBaseUri

    parseUriOrFail s = fromMaybe (fail "") (return <$> parseURI s)

    subscribeTo topic subscriptions =
      getHubLinks subscriptions topic >>= \case
        [] -> putStrLn "No hub found."
        hub:_ -> do
          putStrLn $
            printf "Subscribing to %s through %s." (show topic) (show hub)
          res <- subscribe subscriptions hub topic
          case res of
            Left err -> putStrLn $ printf "Subscription failed: %s" (show err)
            Right () ->
              awaitActiveSubscription subscriptions callbackUri >>=
                \case
                  Left err -> putStrLn $ printf "Subscription failed: %s" (show err)
                  Right notifications ->
                    void $ forkIO $ forever $ do
                      notification <- readChan notifications
                      putStr "Content Type: "
                      print (contentType notification)
                      C.putStrLn (body notification)

    notFound req respond =
      respond $ responseLBS status404 [] "Not Found"
