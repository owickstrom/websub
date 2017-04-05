{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8     as C
import           Data.Monoid                    ((<>))
import           Network.HTTP.WebSub
import           Network.HTTP.WebSub.Subscriber
import           Network.URI

main :: IO ()
main = do
  client <- newClient
  subscribe client (Hub hubUri) (Subscriber callbackUri) $ \topic notification -> do
    print (contentType notification)
    C.putStrLn ("" <> body notification)

  notify client (Subscriber callbackUri) (Topic topicUri) (Notification "" "")
  where
    hubUri = URI "http" (Just (URIAuth "" "some-hub" "")) "/hub" "" ""
    callbackUri = URI "http" (Just (URIAuth "" "localhost" ":9000")) "/notifications" "" ""
    topicUri = URI "http" (Just (URIAuth "" "some-publisher" "")) "/resource" "" ""
