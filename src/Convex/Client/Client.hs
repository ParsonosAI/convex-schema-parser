{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Convex.Client.Client
  ( ConvexClientConfig (..)
  , ConvexClient (..)
  , withConvexClient
  , authenticate
  , convexQuery
  , convexMutation
  , convexAction
  ) where

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (bracket, throwIO)
import Control.Monad (forever, when)
import Convex.Client.Types
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecode, encode, withObject, (.:))
import Data.Aeson.Types (Pair)
import Data.Bifunctor (bimap)
import Data.Default (Default (..))
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Network.WebSockets (Connection)
import System.IO.Error (userError)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS
import qualified Wuss

-- | Configuration for the Convex client.
data ConvexClientConfig = ConvexClientConfig
  { url :: !Text
  , deploymentKey :: !Text
  }

instance Default ConvexClientConfig where
  def = ConvexClientConfig
    { url = "localhost:8888"
    , deploymentKey = ""
    }

-- | The Convex client handle.
data ConvexClient = ConvexClient
  { connection :: !Connection
  , inFlightRequests :: !(TVar (Map UUID (MVar Value)))
  }

-- | The main entry point for using the Convex client.
withConvexClient :: ConvexClientConfig -> (ConvexClient -> IO a) -> IO a
withConvexClient config action =
  Wuss.runSecureClient (show config.url) 443 "/" $ \conn -> do
    inFlight <- newTVarIO Map.empty
    let client = ConvexClient conn inFlight
    bracket (async $ listener client) cancel $ \_ -> do
      -- Conditionally perform initial authentication
      when (not $ T.null config.deploymentKey) $
        authenticate client config.deploymentKey
      action client

-- | The response from the server.
data ServerResponse = ServerResponse
  { requestId :: UUID
  , result :: Value
  } deriving stock (Show, Eq, Generic)

instance FromJSON ServerResponse where
  parseJSON = withObject "ServerResponse" $ \v -> ServerResponse
    <$> v .: "requestId"
    <*> v .: "result"

-- | Listens for messages from the server and fulfills the in-flight requests.
listener :: ConvexClient -> IO ()
listener client = forever $ do
  msg <- WS.receiveData client.connection
  case eitherDecode msg of
    Left err -> print $ "Failed to decode server message: " <> err
    Right (ServerResponse reqId result) -> do
      maybeMVar <- atomically $ do
        reqs <- readTVar client.inFlightRequests
        let maybeMVar = Map.lookup reqId reqs
        writeTVar client.inFlightRequests (Map.delete reqId reqs)
        return maybeMVar
      case maybeMVar of
        Nothing -> print $ "Received response for unknown request ID: " <> show reqId
        Just mvar -> putMVar mvar result

-- | Sends a request to the server and waits for the response.
sendRequest :: ToJSON a => ConvexClient -> a -> [Pair] -> IO Value
sendRequest client req extraFields = do
  reqId <- nextRandom
  mvar <- newEmptyMVar
  atomically $ do
    reqs <- readTVar client.inFlightRequests
    writeTVar client.inFlightRequests (Map.insert reqId mvar reqs)
  let val = toJSON req
  case val of
    (Object obj) ->
      let obj' = foldr (uncurry KeyMap.insert) obj (("requestId", toJSON reqId) : extraFields)
       in WS.sendTextData client.connection (encode (Object obj'))
    _ -> error "Request must be a JSON object"
  takeMVar mvar

-- | Authenticates the client with the given token.
authenticate :: ConvexClient -> Text -> IO ()
authenticate client token = do
  res <- sendRequest client (AuthenticateRequest token) [("type", "authenticate")]
  case res of
    Object obj ->
      case KeyMap.lookup "status" obj of
        Just (String "ok") -> pure ()
        _ -> throwIO . userError $ "Authentication failed: " <> show res
    _ -> throwIO . userError $ "Invalid authentication response: " <> show res

-- | API functions
convexQuery :: ConvexClient -> Text -> Value -> IO (Either Text Value)
convexQuery client path args =
  bimap (.errorMessage) (.value) . eitherDecode . encode <$>
    sendRequest client (QueryRequest path args) [("type", "query")]

convexMutation :: ConvexClient -> Text -> Value -> IO (Either Text Value)
convexMutation client path args =
  bimap (.errorMessage) (.value) . eitherDecode . encode <$>
    sendRequest client (MutationRequest path args) [("type", "mutation")]

convexAction :: ConvexClient -> Text -> Value -> IO (Either Text Value)
convexAction client path args =
  bimap (.errorMessage) (.value) . eitherDecode . encode <$>
    sendRequest client (ActionRequest path args) [("type", "action")]
