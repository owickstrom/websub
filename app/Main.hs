{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8     as C
import           Data.Maybe
import           Data.Monoid                    ((<>))
import           Network.HTTP.Media.MediaType   (MediaType, (//))
import           Network.HTTP.WebSub
import           Network.HTTP.WebSub.Subscriber
import           Network.URI

import           System.Environment
import           System.IO                      (hPutStrLn, stderr)
import Text.Printf

usage :: IO ()
usage =
  hPutStrLn stderr "Usage: websub-exe TOPIC_URI"

main :: IO ()
main =
  getArgs >>=
    \case
      [topicUri] -> do
        topic <- Topic <$> parseUriOrFail topicUri
        client <- newClient clientBaseUri
        getHubLinks client topic >>=
          \case
            [] -> putStrLn "No hub found."
            hub : _ -> do
              putStrLn $ printf "Subscribing to %s through %s." (show topic) (show hub)
              subscribe client hub topic $ \notification -> do
                putStr "Content Type: "
                print (contentType notification)
                C.putStrLn (body notification)

        -- notify client callbackUri (Notification hub topic ("text" // "plain") "omg")
        -- notify client callbackUri (Notification hub topic ("text" // "html") "<h1>Cool</h1>")
        -- return ()
      args -> usage

  where

    clientBaseUri =
      URI "http" (Just (URIAuth "" "localhost" ":3000")) "/subscriptions" "" ""

    hub (Topic topicUri) =
      -- Hack for now, pleases websub.rocks URI scheme.
      Hub $ topicUri { uriPath = uriPath topicUri ++ "/hub" }

    callbackUri =
      CallbackURI clientBaseUri

    parseUriOrFail s =
      fromMaybe (fail "") (return <$> parseURI s)
