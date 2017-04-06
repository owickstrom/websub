{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.WebSub.Subscriber
       ( Client
       , newClient
       , subscribe
       , getHubLinks
       , notify
       ) where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad                (forever)

import qualified Data.ByteString.Char8        as C
import qualified Data.ByteString.Lazy         as LBS
import           Data.Function                ((&))
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HM
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (Text)

import           Network.HTTP.Link.Parser     (parseLinkHeaderBS)
import           Network.HTTP.Link.Types      (Link (..), LinkParam (..),
                                               linkParams)
import           Network.HTTP.Media.MediaType (MediaType)
import           Network.HTTP.Simple          as HTTP
import           Network.HTTP.Types.Status    (status202)
import           Network.HTTP.WebSub
import           Network.URI                  (URI, uriAuthority, uriRegName,
                                               uriScheme, uriToString)

import           Web.FormUrlEncoded

data Client
  = Client { baseUri     :: URI
           , subscribers :: MVar (HashMap CallbackURI (Chan Notification))
           }


newClient :: URI -> IO Client
newClient baseUri =
  Client baseUri <$> newMVar HM.empty

createSubscriberChan :: Client
                     -> CallbackURI
                     -> IO (Chan Notification)
createSubscriberChan client callbackUri = do
  chan <- newChan
  modifyMVar_ (subscribers client) (return . HM.insert callbackUri chan)
  return chan


findSubscriberChan :: Client
                   -> CallbackURI
                   -> IO (Maybe (Chan Notification))
findSubscriberChan client uri = do
  chans <- readMVar (subscribers client)
  return (HM.lookup uri chans)


data SubscribeError
  = InvalidHub Hub
  | HTTPError (HTTP.Response LBS.ByteString)


requestFromUri :: URI -> Maybe Request
requestFromUri uri =
  parseRequest (uriToString id uri "")


requestSubscription :: Client
                    -> Hub
                    -> SubscriptionRequest
                    -> IO (Either SubscribeError ())
requestSubscription client hub subReq =
  case makeHubRequest hub subReq of
    Just req -> do
      LBS.putStrLn (urlEncodeAsForm subReq)
      res <- httpLBS req
      print res
      if getResponseStatus res == status202
         then return (Right ())
         else return (Left (HTTPError res))
    Nothing   -> return (Left (InvalidHub hub))
  where
    makeHubRequest (Hub hub) subReq =
      setRequestMethod "POST"
      . setRequestBodyLBS (urlEncodeAsForm subReq)
      <$> requestFromUri hub


type NotificationCallback = Notification -> IO ()


subscribe :: Client
          -> Hub
          -> Topic
          -> NotificationCallback
          -> IO ()
subscribe client hub topic onNotification = do
  let callbackUri = CallbackURI (baseUri client)
      subReq = SubscriptionRequest callbackUri Subscribe topic
  chan <- createSubscriberChan client callbackUri
  requestSubscription client hub subReq
  forkIO $ forever (readChan chan >>= onNotification)
  return ()


getHubLinks :: Client
            -> Topic
            -> IO [Hub]
getHubLinks client (Topic uri) =
  case setRequestMethod "HEAD" <$> requestFromUri uri of
    Just req -> do
      res <- httpNoBody req
      return (hubLinks (getResponseHeader "Link" res))
    Nothing ->
      return []
  where
    isHubLink link =
      lookup Rel (linkParams link) == Just "hub"

    hubLinks :: [C.ByteString] -> [Hub]
    hubLinks headers =
      headers
      & map parseLinkHeaderBS
      & catMaybes
      & concat
      & filter isHubLink
      & map toHub

    toHub (Link uri _) =
      Hub uri


notify :: Client
       -> CallbackURI
       -> Notification
       -> IO Bool
notify client callbackUri notification =
  findSubscriberChan client callbackUri >>=
    \case
      Just chan ->
        writeChan chan notification *> return True
      Nothing ->
        return False
