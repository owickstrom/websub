{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8     as C
import           Data.Monoid                    ((<>))
import           Network.HTTP.Media.MediaType   (MediaType, (//))
import           Network.HTTP.WebSub
import           Network.HTTP.WebSub.Subscriber
import           Network.URI

main :: IO ()
main = do
  client <- newClient
  subscribe client hub subReq $ \notification -> do
    putStr "Content Type: "
    print (contentType notification)
    C.putStrLn (body notification)

  notify client callbackUri (Notification topic ("text" // "plain") "omg")
  notify client callbackUri (Notification topic ("text" // "html") "<h1>Cool</h1>")

  return ()

  where
    hub =
      Hub $ URI "http" (Just (URIAuth "" "some-hub" "")) "/hub" "" ""
    callbackUri =
      CallbackURI $ URI "http" (Just (URIAuth "" "localhost" ":9000")) "/notifications" "" ""
    topic =
      Topic $ URI "http" (Just (URIAuth "" "some-publisher" "")) "/resource" "" ""

    subReq = SubscriptionRequest (Subscriber callbackUri) Subscribe
