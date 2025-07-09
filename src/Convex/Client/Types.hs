{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Convex.Client.Types where

import Control.Concurrent.MVar (MVar)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

-- | Represents the result of a function call (mutation or action).
data FunctionResult
  = Success { value :: Value }
  | Error { errorMessage :: Text }
  deriving stock (Show, Eq, Generic)

instance FromJSON FunctionResult
instance ToJSON FunctionResult

-- | Represents an active query subscription.
-- Placeholder for now.
data QuerySubscription = QuerySubscription
  deriving stock (Show, Eq, Generic)

instance FromJSON QuerySubscription
instance ToJSON QuerySubscription

-- | Request to authenticate the client.
data AuthenticateRequest = AuthenticateRequest
  { token :: Text
  } deriving stock (Show, Eq, Generic)

instance ToJSON AuthenticateRequest

-- | Request to execute a query.
data QueryRequest = QueryRequest
  { path :: Text
  , args :: Value
  } deriving stock (Show, Eq, Generic)

instance ToJSON QueryRequest

-- | Request to execute a mutation.
data MutationRequest = MutationRequest
  { path :: Text
  , args :: Value
  } deriving stock (Show, Eq, Generic)

instance ToJSON MutationRequest

-- | Request to execute an action.
data ActionRequest = ActionRequest
  { path :: Text
  , args :: Value
  } deriving stock (Show, Eq, Generic)

instance ToJSON ActionRequest

-- | Request to subscribe to a query.
data SubscribeRequest = SubscribeRequest
  { path :: Text
  , args :: Value
  } deriving stock (Show, Eq, Generic)

instance ToJSON SubscribeRequest

-- | Request to unsubscribe from a query.
data UnsubscribeRequest = UnsubscribeRequest
  { queryId :: UUID -- Assuming the server tracks subscriptions by an ID.
  } deriving stock (Show, Eq, Generic)

instance ToJSON UnsubscribeRequest

-- | A sum type representing all possible requests a client can send.
-- The type parameters represent the channels for receiving responses.
data ClientRequest
  = Mutation MutationRequest (MVar FunctionResult)
  | Action ActionRequest (MVar FunctionResult)
  | Subscribe SubscribeRequest (MVar QuerySubscription) -- Simplified for now
  | Unsubscribe UnsubscribeRequest
  | Authenticate AuthenticateRequest
