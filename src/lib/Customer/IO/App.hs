{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Customer.IO.App (
    -- * Client
    CustomerIOAppClient (..),
    createAppClient,
    sendTransactionalEmail,
    triggerBroadcast,

    -- * Types
    CustomerIOAppAuth (..),
    SendEmailRequest (..),
    EmailIdentifier (..),
    TransactionalMessageId (..),
    SendEmailResponse (..),
    BroadcastId (..),
    TriggerBroadcastRequest (..),
    TriggerId (..),

    -- * Errors
    CustomerIOAppError (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API (
    Capture,
    Header,
    Header',
    JSON,
    Post,
    ReqBody,
    Required,
    Strict,
    ToHttpApiData,
    (:>),
 )
import Servant.Client (
    ClientEnv,
    ClientError,
    ClientM,
    client,
    mkClientEnv,
    parseBaseUrl,
    runClientM,
 )


-- | Authentication credentials for Customer.io App API
newtype CustomerIOAppAuth = CustomerIOAppAuth
    { bearerToken :: Text
    -- ^ Your Customer.io App API Bearer Token
    }
    deriving (Show)


-- | How to identify the email recipient
data EmailIdentifier = EmailIdentifier
    { recipientEmail :: Text
    -- ^ Email address of recipient
    , recipientId :: Maybe Text
    -- ^ Optional customer ID
    , recipientCioId :: Maybe Text
    -- ^ Optional Customer.io internal ID
    }
    deriving (Show, Generic)


instance ToJSON EmailIdentifier where
    toJSON (EmailIdentifier email maybeId maybeCioId) =
        Aeson.object $
            concat
                [ ["email" Aeson..= email]
                , maybe [] (\id' -> ["id" Aeson..= id']) maybeId
                , maybe [] (\cioId -> ["cio_id" Aeson..= cioId]) maybeCioId
                ]


-- | Template identifier - can be numeric ID or trigger name
data TransactionalMessageId
    = -- | Numeric template ID
      TemplateId Int
    | -- | Template trigger name
      TriggerName Text
    deriving (Show)


instance ToJSON TransactionalMessageId where
    toJSON (TemplateId tid) = Aeson.toJSON tid
    toJSON (TriggerName name) = Aeson.toJSON name


-- | Simplified request for sending transactional emails
data SendEmailRequest = SendEmailRequest
    { templateId :: TransactionalMessageId
    -- ^ Template to use
    , identifiers :: EmailIdentifier
    -- ^ Who to send to
    , messageData :: Maybe (Map Text Aeson.Value)
    -- ^ Template variables
    , sendAt :: Maybe Int
    -- ^ Unix timestamp to send (optional)
    }
    deriving (Show, Generic)


instance ToJSON SendEmailRequest where
    toJSON (SendEmailRequest tid idents mData mSendAt) =
        Aeson.object $
            concat
                [ ["transactional_message_id" Aeson..= tid]
                , ["identifiers" Aeson..= idents]
                , ["to" Aeson..= recipientEmail idents] -- Required field
                , maybe [] (\d -> ["message_data" Aeson..= d]) mData
                , maybe [] (\s -> ["send_at" Aeson..= s]) mSendAt
                ]


-- | Response from sending an email
newtype SendEmailResponse = SendEmailResponse
    { deliveryId :: Text
    -- ^ Unique delivery identifier
    }
    deriving (Show, Generic)


instance FromJSON SendEmailResponse where
    parseJSON = Aeson.withObject "SendEmailResponse" $ \o ->
        SendEmailResponse <$> o Aeson..: "delivery_id"


-- | Identifier of an API-triggered broadcast, as shown in the Customer.io UI.
newtype BroadcastId = BroadcastId
    { unBroadcastId :: Int
    }
    deriving (Eq, Show, ToHttpApiData)


-- | Request for triggering an API-triggered broadcast. The broadcast is always sent
-- to the default audience configured for it in the Customer.io UI.
--
-- Note that we do not support the optional fields of the endpoint other than @data@
-- (e.g. @recipients@, @ids@, @emails@, @per_user_data@, @data_file_url@, @scheduled_at@, @rate_limit@).
newtype TriggerBroadcastRequest = TriggerBroadcastRequest
    { broadcastData :: Maybe (Map Text Aeson.Value)
    -- ^ Values available in the message via liquid, as @{{trigger.<key>}}@
    }
    deriving (Show, Generic)


instance ToJSON TriggerBroadcastRequest where
    toJSON (TriggerBroadcastRequest mData) =
        Aeson.object $ maybe [] (\d -> ["data" Aeson..= d]) mData


-- | Identifier of a triggered broadcast, which can be used to check its status or errors.
newtype TriggerId = TriggerId
    { unTriggerId :: Int
    }
    deriving (Eq, Show)


instance FromJSON TriggerId where
    parseJSON = Aeson.withObject "TriggerId" $ \o ->
        TriggerId <$> o Aeson..: "id"


-- | Customer.io App API errors
newtype CustomerIOAppError = CustomerIOAppError ClientError
    deriving (Show)


-- | The send email endpoint - POST /v1/send/email
type SendEmailAPI =
    "v1"
        :> "send"
        :> "email"
        :> Header "Authorization" Text
        :> ReqBody '[JSON] SendEmailRequest
        :> Post '[JSON] SendEmailResponse


-- | The trigger broadcast endpoint - POST /v1/campaigns/{broadcast_id}/triggers
--
-- https://docs.customer.io/api/app/#operation/triggerBroadcast
type TriggerBroadcastAPI =
    "v1"
        :> "campaigns"
        :> Capture "broadcast_id" BroadcastId
        :> "triggers"
        :> Header' '[Required, Strict] "Authorization" Text
        :> ReqBody '[JSON] TriggerBroadcastRequest
        :> Post '[JSON] TriggerId


-- | Simple Customer.io App API client
data CustomerIOAppClient = CustomerIOAppClient
    { appClientAuth :: CustomerIOAppAuth
    , appClientEnv :: ClientEnv
    }


-- | Create a Customer.io App API client with hardcoded api.customer.io URL
createAppClient :: CustomerIOAppAuth -> IO CustomerIOAppClient
createAppClient auth = do
    manager <- newManager tlsManagerSettings
    let baseUrl = fromMaybe (error "Invalid hardcoded URL") $ parseBaseUrl "https://api.customer.io"
        env = mkClientEnv manager baseUrl
    return $ CustomerIOAppClient auth env


-- | Send a transactional email using a template
sendTransactionalEmail ::
    CustomerIOAppClient ->
    TransactionalMessageId ->
    EmailIdentifier ->
    Maybe (Map Text Aeson.Value) ->
    IO (Either CustomerIOAppError SendEmailResponse)
sendTransactionalEmail appClient templateId' recipient templateVars = do
    let request = SendEmailRequest templateId' recipient templateVars Nothing
        authHeader = "Bearer " <> bearerToken (appClientAuth appClient)
    result <- runClientM (sendEmailClient (Just authHeader) request) (appClientEnv appClient)
    case result of
        Left err -> pure . Left $ CustomerIOAppError err
        Right response -> pure $ Right response


-- | Internal client function generated by servant-client
sendEmailClient :: Maybe Text -> SendEmailRequest -> ClientM SendEmailResponse
sendEmailClient = client (Proxy :: Proxy SendEmailAPI)


-- | Trigger an API-triggered broadcast.
--
-- Customer.io limits this endpoint to one request every 10 seconds.
--
-- https://docs.customer.io/messaging/send/broadcasts/api-triggered-broadcasts/
triggerBroadcast ::
    CustomerIOAppClient ->
    BroadcastId ->
    TriggerBroadcastRequest ->
    IO (Either CustomerIOAppError TriggerId)
triggerBroadcast appClient broadcastId request = do
    let authHeader = "Bearer " <> bearerToken (appClientAuth appClient)
    result <- runClientM (triggerBroadcastClient broadcastId authHeader request) (appClientEnv appClient)
    pure $ either (Left . CustomerIOAppError) Right result


-- | Internal client function generated by servant-client
triggerBroadcastClient :: 
    BroadcastId -> 
    -- | Authorization
    Text -> 
    TriggerBroadcastRequest -> 
    ClientM TriggerId
triggerBroadcastClient = client (Proxy :: Proxy TriggerBroadcastAPI)
