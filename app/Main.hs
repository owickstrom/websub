{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void, forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe
import Data.Monoid ((<>))
import Network.HTTP.Media.MediaType (MediaType, (//))
import Network.HTTP.WebSub
import Network.HTTP.WebSub.HTTPClient
import Network.HTTP.WebSub.Subscriber
import Network.URI

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
    args -> usage
  where
    clientBaseUri =
      URI "http:" (Just (URIAuth "" "localhost" ":3000")) "/subscriptions" "" ""
    hub (Topic topicUri)
      -- Hack for now, pleases websub.rocks URI scheme.
     = Hub $ topicUri {uriPath = uriPath topicUri ++ "/hub"}
    callbackUri = CallbackURI clientBaseUri
    parseUriOrFail s = fromMaybe (fail "") (return <$> parseURI s)
