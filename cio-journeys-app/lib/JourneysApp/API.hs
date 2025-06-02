{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module JourneysApp.API
  ( -- * Client and Server
    Config(..)
  , JourneysAppBackend(..)
  , createJourneysAppClient
  , runJourneysAppServer
  , runJourneysAppMiddlewareServer
  , runJourneysAppClient
  , runJourneysAppClientWithManager
  , callJourneysApp
  , JourneysAppClient
  , JourneysAppClientError(..)
  -- ** Servant
  , JourneysAppAPI
  -- ** Plain WAI Application
  , serverWaiApplicationJourneysApp
  -- ** Authentication
  , JourneysAppAuth(..)
  , clientAuth
  , Protected
  ) where

import           JourneysApp.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import qualified Data.Aeson                         as Aeson
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy               as BSL
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import           Network.Wai                        (Middleware, Request, requestHeaders)
import qualified Network.Wai.Handler.Warp           as Warp
import           Network.Wai.Middleware.HttpAuth    (extractBearerAuth)
import           Servant                            (ServerError, serveWithContextT, throwError)
import           Servant.API                        hiding (addHeader)
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.API.Experimental.Auth      (AuthProtect)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application, Context ((:.), EmptyContext))
import           Servant.Server.Experimental.Auth   (AuthHandler, AuthServerData, mkAuthHandler)
import           Servant.Server.StaticFiles         (serveDirectoryFileServer)
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList

newtype JSONQueryParam a = JSONQueryParam
  { fromJsonQueryParam :: a
  } deriving (Functor, Foldable, Traversable)

instance Aeson.ToJSON a => ToHttpApiData (JSONQueryParam a) where
  toQueryParam = T.decodeUtf8 . BSL.toStrict . Aeson.encode . fromJsonQueryParam

instance Aeson.FromJSON a => FromHttpApiData (JSONQueryParam a) where
  parseQueryParam = either (Left . T.pack) (Right . JSONQueryParam) . Aeson.eitherDecodeStrict . T.encodeUtf8


-- | Servant type-level API, generated from the OpenAPI spec for JourneysApp.
type JourneysAppAPI
    =    Protected :> "v1" :> "activities" :> QueryParam "start" Text :> QueryParam "type" Text :> QueryParam "name" Text :> QueryParam "deleted" Bool :> QueryParam "customer_id" Text :> QueryParam "id_type" Text :> QueryParam "limit" Int :> Verb 'GET 200 '[JSON] ListActivities200Response -- 'listActivities' route
    :<|> Protected :> "v1" :> "broadcasts" :> Capture "broadcast_id" Int :> "actions" :> Capture "action_id" Int :> "metrics" :> "links" :> QueryParam "period" Text :> QueryParam "steps" Int :> QueryParam "type" Text :> Verb 'GET 200 '[JSON] BroadcastActionLinks200Response -- 'broadcastActionLinks' route
    :<|> Protected :> "v1" :> "broadcasts" :> Capture "broadcast_id" Int :> "actions" :> Capture "action_id" Int :> "metrics" :> QueryParam "period" Text :> QueryParam "steps" Int :> QueryParam "type" Text :> Verb 'GET 200 '[JSON] BroadcastMetrics200Response -- 'broadcastActionMetrics' route
    :<|> Protected :> "v1" :> "broadcasts" :> Capture "broadcast_id" Int :> "actions" :> Verb 'GET 200 '[JSON] BroadcastActions200Response -- 'broadcastActions' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "broadcast_id" Int :> "triggers" :> Capture "trigger_id" Int :> "errors" :> QueryParam "start" Text :> QueryParam "limit" Int :> Verb 'GET 200 '[JSON] BroadcastErrors200Response -- 'broadcastErrors' route
    :<|> Protected :> "v1" :> "broadcasts" :> Capture "broadcast_id" Int :> "metrics" :> "links" :> QueryParam "period" Text :> QueryParam "steps" Int :> QueryParam "unique" Bool :> Verb 'GET 200 '[JSON] BroadcastLinks200Response -- 'broadcastLinks' route
    :<|> Protected :> "v1" :> "broadcasts" :> Capture "broadcast_id" Int :> "messages" :> QueryParam "start" Text :> QueryParam "limit" Int :> QueryParam "metric" Text :> QueryParam "state" Text :> QueryParam "type" Text :> QueryParam "start_ts" Int :> QueryParam "end_ts" Int :> Verb 'GET 200 '[JSON] BroadcastMessages200Response -- 'broadcastMessages' route
    :<|> Protected :> "v1" :> "broadcasts" :> Capture "broadcast_id" Int :> "metrics" :> QueryParam "period" Text :> QueryParam "steps" Int :> QueryParam "type" Text :> Verb 'GET 200 '[JSON] BroadcastMetrics200Response -- 'broadcastMetrics' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "broadcast_id" Int :> "triggers" :> Capture "trigger_id" Int :> Verb 'GET 200 '[JSON] BroadcastStatus200Response -- 'broadcastStatus' route
    :<|> Protected :> "v1" :> "broadcasts" :> Capture "broadcast_id" Int :> Verb 'GET 200 '[JSON] GetBroadcast200Response -- 'getBroadcast' route
    :<|> Protected :> "v1" :> "broadcasts" :> Capture "broadcast_id" Int :> "actions" :> Capture "action_id" Int :> Verb 'GET 200 '[JSON] GetBroadcastAction200Response -- 'getBroadcastAction' route
    :<|> Protected :> "v1" :> "broadcasts" :> Capture "broadcast_id" Int :> "actions" :> Capture "action_id" Int :> "language" :> Capture "language" Text :> Verb 'GET 200 '[JSON] GetBroadcastAction200Response -- 'getBroadcastActionLanguage' route
    :<|> Protected :> "v1" :> "broadcasts" :> Capture "broadcast_id" Int :> "triggers" :> Verb 'GET 200 '[JSON] ListBroadcastTriggers200Response -- 'listBroadcastTriggers' route
    :<|> Protected :> "v1" :> "broadcasts" :> Verb 'GET 200 '[JSON] ListBroadcasts200Response -- 'listBroadcasts' route
    :<|> Protected :> "v1" :> "broadcasts" :> Capture "broadcast_id" Int :> "actions" :> Capture "action_id" Int :> ReqBody '[JSON] BroadcastActions200ResponseActionsInner :> Verb 'PUT 200 '[JSON] GetBroadcastAction200Response -- 'updateBroadcastAction' route
    :<|> Protected :> "v1" :> "broadcasts" :> Capture "broadcast_id" Int :> "actions" :> Capture "action_id" Int :> "language" :> Capture "language" Text :> ReqBody '[JSON] BroadcastActions200ResponseActionsInner :> Verb 'PUT 200 '[JSON] GetBroadcastAction200Response -- 'updateBroadcastActionLanguage' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "campaign_id" Int :> "actions" :> Capture "action_id" Int :> "metrics" :> "links" :> QueryParam "period" Text :> QueryParam "steps" Int :> QueryParam "type" Text :> Verb 'GET 200 '[JSON] CampaignActionLinks200Response -- 'campaignActionLinks' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "campaign_id" Int :> "actions" :> Capture "action_id" Int :> "metrics?version=2" :> QueryParam "res" Text :> QueryParam "tz" Text :> QueryParam "start" Int :> QueryParam "end" Int :> QueryParam "type" Text :> Verb 'GET 200 '[JSON] BroadcastMetrics200Response -- 'campaignActionMetrics' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "campaign_id" Int :> "actions" :> Capture "action_id" Int :> "metrics" :> QueryParam "period" Text :> QueryParam "steps" Int :> QueryParam "type" Text :> Verb 'GET 200 '[JSON] CampaignMetricsDeprecated200Response -- 'campaignActionMetricsDeprecated' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "campaign_id" Int :> "journey_metrics" :> QueryParam "start" Int :> QueryParam "end" Int :> QueryParam "res" Text :> Verb 'GET 200 '[JSON] CampaignJourneyMetrics200Response -- 'campaignJourneyMetrics' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "campaign_id" Int :> "metrics" :> "links" :> QueryParam "period" Text :> QueryParam "steps" Int :> QueryParam "unique" Bool :> Verb 'GET 200 '[JSON] BroadcastLinks200Response -- 'campaignLinkMetrics' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "campaign_id" Int :> "metrics?version=2" :> QueryParam "res" Text :> QueryParam "tz" Text :> QueryParam "start" Int :> QueryParam "end" Int :> QueryParam "type" Text :> Verb 'GET 200 '[JSON] BroadcastMetrics200Response -- 'campaignMetrics' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "campaign_id" Int :> "metrics" :> QueryParam "period" Text :> QueryParam "steps" Int :> QueryParam "type" Text :> Verb 'GET 200 '[JSON] CampaignMetricsDeprecated200Response -- 'campaignMetricsDeprecated' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "campaign_id" Int :> "actions" :> Capture "action_id" Int :> Verb 'GET 200 '[JSON] GetCampaignAction200Response -- 'getCampaignAction' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "campaign_id" Int :> "actions" :> Capture "action_id" Int :> "language" :> Capture "language" Text :> Verb 'GET 200 '[JSON] GetCampaignAction200Response -- 'getCampaignActionTranslation' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "campaign_id" Int :> "messages" :> QueryParam "start" Text :> QueryParam "limit" Int :> QueryParam "type" Text :> QueryParam "metric" Text :> QueryParam "drafts" Bool :> QueryParam "start_ts" Int :> QueryParam "end_ts" Int :> Verb 'GET 200 '[JSON] GetCampaignMessages200Response -- 'getCampaignMessages' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "campaign_id" Int :> Verb 'GET 200 '[JSON] GetCampaigns200Response -- 'getCampaigns' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "campaign_id" Int :> "actions" :> QueryParam "start" Text :> Verb 'GET 200 '[JSON] ListCampaignActions200Response -- 'listCampaignActions' route
    :<|> Protected :> "v1" :> "campaigns" :> Verb 'GET 200 '[JSON] ListCampaigns200Response -- 'listCampaigns' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "campaign_id" Int :> "actions" :> Capture "action_id" Int :> ReqBody '[JSON] ListCampaignActions200ResponseActionsInner :> Verb 'PUT 200 '[JSON] GetCampaignAction200Response -- 'updateCampaignAction' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "campaign_id" Int :> "actions" :> Capture "action_id" Int :> "language" :> Capture "language" Text :> ReqBody '[JSON] ListCampaignActions200ResponseActionsInner :> Verb 'PUT 200 '[JSON] GetCampaignAction200Response -- 'updateCampaignActionTranslation' route
    :<|> Protected :> "v1" :> "collections" :> ReqBody '[JSON] AddCollectionRequest :> Verb 'POST 200 '[JSON] AddCollection200Response -- 'addCollection' route
    :<|> Protected :> "v1" :> "collections" :> Capture "collection_id" Int :> Verb 'DELETE 204 '[JSON] NoContent -- 'deleteCollection' route
    :<|> Protected :> "v1" :> "collections" :> Capture "collection_id" Int :> Verb 'GET 200 '[JSON] AddCollection200Response -- 'getCollection' route
    :<|> Protected :> "v1" :> "collections" :> Capture "collection_id" Int :> "content" :> Verb 'GET 200 '[JSON] ((Map.Map String Value)) -- 'getCollectionContents' route
    :<|> Protected :> "v1" :> "collections" :> Verb 'GET 200 '[JSON] GetCollections200Response -- 'getCollections' route
    :<|> Protected :> "v1" :> "collections" :> Capture "collection_id" Int :> ReqBody '[JSON] UpdateCollectionRequest :> Verb 'PUT 200 '[JSON] AddCollection200Response -- 'updateCollection' route
    :<|> Protected :> "v1" :> "collections" :> Capture "collection_id" Int :> "content" :> ReqBody '[JSON] (Map.Map String Value) :> Verb 'PUT 200 '[JSON] AddCollection200Response -- 'updateCollectionContents' route
    :<|> Protected :> "v1" :> "customers" :> "attributes" :> ReqBody '[JSON] GetPeopleByIdRequest :> Verb 'POST 200 '[JSON] GetPeopleById200Response -- 'getPeopleById' route
    :<|> Protected :> "v1" :> "customers" :> QueryParam "email" Text :> Verb 'GET 200 '[JSON] GetPeopleEmail200Response -- 'getPeopleEmail' route
    :<|> Protected :> "v1" :> "customers" :> QueryParam "start" Text :> QueryParam "limit" Int :> ReqBody '[JSON] GetPeopleFilterRequest :> Verb 'POST 200 '[JSON] GetPeopleFilter200Response -- 'getPeopleFilter' route
    :<|> Protected :> "v1" :> "customers" :> Capture "customer_id" Text :> "activities" :> QueryParam "id_type" Text :> QueryParam "start" Text :> QueryParam "limit" Int :> QueryParam "type" Text :> QueryParam "name" Text :> Verb 'GET 200 '[JSON] GetPersonActivities200Response -- 'getPersonActivities' route
    :<|> Protected :> "v1" :> "customers" :> Capture "customer_id" Text :> "attributes" :> QueryParam "id_type" Text :> Verb 'GET 200 '[JSON] GetPersonAttributes200Response -- 'getPersonAttributes' route
    :<|> Protected :> "v1" :> "customers" :> Capture "customer_id" Text :> "messages" :> QueryParam "id_type" Text :> QueryParam "start" Text :> QueryParam "limit" Int :> QueryParam "start_ts" Int :> QueryParam "end_ts" Int :> Verb 'GET 200 '[JSON] GetPersonMessages200Response -- 'getPersonMessages' route
    :<|> Protected :> "v1" :> "customers" :> Capture "customer_id" Text :> "relationships" :> QueryParam "start" Text :> QueryParam "limit" Int :> Verb 'GET 200 '[JSON] GetPersonRelationships200Response -- 'getPersonRelationships' route
    :<|> Protected :> "v1" :> "customers" :> Capture "customer_id" Text :> "segments" :> QueryParam "id_type" Text :> Verb 'GET 200 '[JSON] GetPersonSegments200Response -- 'getPersonSegments' route
    :<|> Protected :> "v1" :> "customers" :> Capture "customer_id" Text :> "subscription_preferences" :> QueryParam "id_type" Text :> QueryParam "language" Text :> Header "Accept-Language" Text :> Verb 'GET 200 '[JSON] GetPersonSubscriptionPreferences200Response -- 'getPersonSubscriptionPreferences' route
    :<|> Protected :> "v1" :> "esp" :> "suppression" :> Capture "suppression_type" Text :> Capture "email_address" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'deleteSuppression' route
    :<|> Protected :> "v1" :> "esp" :> "search_suppression" :> Capture "email_address" Text :> Verb 'GET 200 '[JSON] GetSuppression200Response -- 'getSuppression' route
    :<|> Protected :> "v1" :> "esp" :> "suppression" :> Capture "suppression_type" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Verb 'GET 200 '[JSON] GetSuppression200Response -- 'getSuppressionByType' route
    :<|> Protected :> "v1" :> "esp" :> "suppression" :> Capture "suppression_type" Text :> Capture "email_address" Text :> Verb 'POST 200 '[JSON] Value -- 'postSuppression' route
    :<|> "v1" :> "info" :> "ip_addresses" :> Verb 'GET 200 '[JSON] GetCioAllowlist200Response -- 'getCioAllowlist' route
    :<|> Protected :> "v1" :> "messages" :> Capture "message_id" Text :> "archived_message" :> Verb 'GET 200 '[JSON] GetArchivedMessage200Response -- 'getArchivedMessage' route
    :<|> Protected :> "v1" :> "messages" :> Capture "message_id" Text :> Verb 'GET 200 '[JSON] GetMessage200Response -- 'getMessage' route
    :<|> Protected :> "v1" :> "messages" :> QueryParam "start" Text :> QueryParam "limit" Int :> QueryParam "type" Text :> QueryParam "metric" Text :> QueryParam "drafts" Bool :> QueryParam "campaign_id" Int :> QueryParam "newsletter_id" Int :> QueryParam "action_id" Int :> QueryParam "start_ts" Int :> QueryParam "end_ts" Int :> Verb 'GET 200 '[JSON] ListMessages200Response -- 'listMessages' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> Verb 'DELETE 204 '[JSON] NoContent -- 'deletetNewsletters' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> "metrics" :> "links" :> QueryParam "period" Text :> QueryParam "steps" Int :> QueryParam "unique" Bool :> Verb 'GET 200 '[JSON] BroadcastLinks200Response -- 'getNewsletterLinks' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> "metrics" :> QueryParam "period" Text :> QueryParam "steps" Int :> QueryParam "type" Text :> Verb 'GET 200 '[JSON] BroadcastMetrics200Response -- 'getNewsletterMetrics' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> "messages" :> QueryParam "start" Text :> QueryParam "limit" Int :> QueryParam "metric" Text :> QueryParam "start_ts" Int :> QueryParam "end_ts" Int :> Verb 'GET 200 '[JSON] GetNewsletterMsgMeta200Response -- 'getNewsletterMsgMeta' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> "test_groups" :> Verb 'GET 200 '[JSON] GetNewsletterTestGroups200Response -- 'getNewsletterTestGroups' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> "contents" :> Capture "content_id" Int :> Verb 'GET 200 '[JSON] GetNewsletterVariant200Response -- 'getNewsletterVariant' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> "language" :> Capture "language" Text :> Verb 'GET 200 '[JSON] GetNewsletterVariant200Response -- 'getNewsletterVariantTranslation' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> "test_group" :> Capture "test_group_id" Text :> "language" :> Capture "language" Text :> Verb 'GET 200 '[JSON] GetNewsletterVariant200Response -- 'getNewsletterVariantTranslationTest' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> Verb 'GET 200 '[JSON] GetNewsletters200Response -- 'getNewsletters' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> "contents" :> Capture "content_id" Int :> "metrics" :> "links" :> QueryParam "period" Text :> QueryParam "steps" Int :> QueryParam "type" Text :> Verb 'GET 200 '[JSON] GetVariantLinks200Response -- 'getVariantLinks' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> "contents" :> Capture "content_id" Int :> "metrics" :> QueryParam "period" Text :> QueryParam "steps" Int :> QueryParam "type" Text :> Verb 'GET 200 '[JSON] BroadcastMetrics200Response -- 'getVariantMetrics' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> "contents" :> Verb 'GET 200 '[JSON] ListNewsletterVariants200Response -- 'listNewsletterVariants' route
    :<|> Protected :> "v1" :> "newsletters" :> QueryParam "limit" Int :> QueryParam "sort" Text :> QueryParam "start" Text :> Verb 'GET 200 '[JSON] ListNewsletters200Response -- 'listNewsletters' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> "test_group" :> Capture "test_group_id" Text :> "language" :> Capture "language" Text :> ReqBody '[JSON] ListNewsletterVariants200ResponseContentsInner :> Verb 'PUT 200 '[JSON] GetNewsletterVariant200Response -- 'updateNewsletterTestTranslation' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> "contents" :> Capture "content_id" Int :> ReqBody '[JSON] ListNewsletterVariants200ResponseContentsInner :> Verb 'PUT 200 '[JSON] GetNewsletterVariant200Response -- 'updateNewsletterVariant' route
    :<|> Protected :> "v1" :> "newsletters" :> Capture "newsletter_id" Int :> "language" :> Capture "language" Text :> ReqBody '[JSON] ListNewsletterVariants200ResponseContentsInner :> Verb 'PUT 200 '[JSON] GetNewsletterVariant200Response -- 'updateNewsletterVariantTranslation' route
    :<|> Protected :> "v1" :> "objects" :> Capture "object_type_id" Int :> Capture "object_id" Text :> "attributes" :> QueryParam "id_type" Text :> Verb 'GET 200 '[JSON] GetObjectAttributes200Response -- 'getObjectAttributes' route
    :<|> Protected :> "v1" :> "objects" :> Capture "object_type_id" Int :> Capture "object_id" Text :> "relationships" :> QueryParam "start" Text :> QueryParam "limit" Int :> QueryParam "id_type" Text :> Verb 'GET 200 '[JSON] GetObjectRelationships200Response -- 'getObjectRelationships' route
    :<|> Protected :> "v1" :> "object_types" :> Verb 'GET 200 '[JSON] GetObjectTypes200Response -- 'getObjectTypes' route
    :<|> Protected :> "v1" :> "objects" :> QueryParam "start" Text :> QueryParam "limit" Int :> ReqBody '[JSON] GetObjectsFilterRequest :> Verb 'POST 200 '[JSON] GetObjectsFilter200Response -- 'getObjectsFilter' route
    :<|> Protected :> "v1" :> "reporting_webhooks" :> ReqBody '[JSON] ListWebhooks200ResponseReportingWebhooksInner :> Verb 'POST 200 '[JSON] ListWebhooks200ResponseReportingWebhooksInner -- 'createWebhook' route
    :<|> Protected :> "v1" :> "reporting_webhooks" :> Capture "webhook_id" Int :> Verb 'DELETE 200 '[JSON] NoContent -- 'deleteWebhook' route
    :<|> Protected :> "v1" :> "reporting_webhooks" :> Capture "webhook_id" Int :> Verb 'GET 200 '[JSON] ListWebhooks200ResponseReportingWebhooksInner -- 'getWebhook' route
    :<|> Protected :> "v1" :> "reporting_webhooks" :> Verb 'GET 200 '[JSON] ListWebhooks200Response -- 'listWebhooks' route
    :<|> Protected :> "v1" :> "reporting_webhooks" :> Capture "webhook_id" Int :> ReqBody '[JSON] ListWebhooks200ResponseReportingWebhooksInner :> Verb 'PUT 200 '[JSON] ListWebhooks200ResponseReportingWebhooksInner -- 'updateWebhook' route
    :<|> Protected :> "v1" :> "segments" :> ReqBody '[JSON] CreateManSegmentRequest :> Verb 'POST 200 '[JSON] CreateManSegment200Response -- 'createManSegment' route
    :<|> Protected :> "v1" :> "segments" :> Capture "segment_id" Int :> Verb 'DELETE 204 '[JSON] NoContent -- 'deleteManSegment' route
    :<|> Protected :> "v1" :> "segments" :> Capture "segment_id" Int :> Verb 'GET 200 '[JSON] CreateManSegment200Response -- 'getSegment' route
    :<|> Protected :> "v1" :> "segments" :> Capture "segment_id" Int :> "customer_count" :> Verb 'GET 200 '[JSON] GetSegmentCount200Response -- 'getSegmentCount' route
    :<|> Protected :> "v1" :> "segments" :> Capture "segment_id" Int :> "used_by" :> Verb 'GET 200 '[JSON] GetSegmentDependencies200Response -- 'getSegmentDependencies' route
    :<|> Protected :> "v1" :> "segments" :> Capture "segment_id" Int :> "membership" :> QueryParam "start" Text :> QueryParam "limit" Int :> Verb 'GET 200 '[JSON] GetSegmentMembership200Response -- 'getSegmentMembership' route
    :<|> Protected :> "v1" :> "segments" :> Verb 'GET 200 '[JSON] ListSegments200Response -- 'listSegments' route
    :<|> Protected :> "v1" :> "send" :> "email" :> ReqBody '[JSON] SendEmailRequest :> Verb 'POST 200 '[JSON] SendEmail200Response -- 'sendEmail' route
    :<|> Protected :> "v1" :> "send" :> "push" :> ReqBody '[JSON] SendPushRequest :> Verb 'POST 200 '[JSON] SendEmail200Response -- 'sendPush' route
    :<|> Protected :> "v1" :> "campaigns" :> Capture "broadcast_id" Int :> "triggers" :> ReqBody '[JSON] TriggerBroadcastRequest :> Verb 'POST 200 '[JSON] TriggerBroadcast200Response -- 'triggerBroadcast' route
    :<|> Protected :> "v1" :> "sender_identities" :> Capture "sender_id" Int :> Verb 'GET 200 '[JSON] GetSender200Response -- 'getSender' route
    :<|> Protected :> "v1" :> "sender_identities" :> Capture "sender_id" Int :> "used_by" :> Verb 'GET 200 '[JSON] GetSenderUsage200Response -- 'getSenderUsage' route
    :<|> Protected :> "v1" :> "sender_identities" :> QueryParam "start" Text :> QueryParam "limit" Int :> QueryParam "sort" Text :> Verb 'GET 200 '[JSON] ListSenders200Response -- 'listSenders' route
    :<|> Protected :> "v1" :> "snippets" :> Capture "snippet_name" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'deleteSnippet' route
    :<|> Protected :> "v1" :> "snippets" :> Verb 'GET 200 '[JSON] ListSnippets200Response -- 'listSnippets' route
    :<|> Protected :> "v1" :> "snippets" :> ReqBody '[JSON] ListSnippets200ResponseSnippetsInner :> Verb 'PUT 200 '[JSON] UpdateSnippets200Response -- 'updateSnippets' route
    :<|> Protected :> "v1" :> "subscription_topics" :> Verb 'GET 200 '[JSON] GetTopics200Response -- 'getTopics' route
    :<|> Protected :> "v1" :> "transactional" :> Capture "transactional_id" Int :> Verb 'GET 200 '[JSON] GetTransactional200Response -- 'getTransactional' route
    :<|> Protected :> "v1" :> "transactional" :> Capture "transactional_id" Int :> "language" :> Capture "language" Text :> Verb 'GET 200 '[JSON] GetTransactionalVariant200Response -- 'getTransactionalVariant' route
    :<|> Protected :> "v1" :> "transactional" :> Verb 'GET 200 '[JSON] ListTransactional200Response -- 'listTransactional' route
    :<|> Protected :> "v1" :> "transactional" :> Capture "transactional_id" Int :> "contents" :> Verb 'GET 200 '[JSON] ListTransactionalVariants200Response -- 'listTransactionalVariants' route
    :<|> Protected :> "v1" :> "transactional" :> Capture "transactional_id" Int :> "metrics" :> "links" :> QueryParam "period" Text :> QueryParam "steps" Int :> QueryParam "unique" Bool :> Verb 'GET 200 '[JSON] BroadcastLinks200Response -- 'transactionalLinks' route
    :<|> Protected :> "v1" :> "transactional" :> Capture "transactional_id" Int :> "messages" :> QueryParam "start" Text :> QueryParam "limit" Int :> QueryParam "metric" Text :> QueryParam "state" Text :> QueryParam "start_ts" Int :> QueryParam "end_ts" Int :> Verb 'GET 200 '[JSON] TransactionalMessages200Response -- 'transactionalMessages' route
    :<|> Protected :> "v1" :> "transactional" :> Capture "transactional_id" Int :> "metrics" :> QueryParam "period" Text :> QueryParam "steps" Int :> Verb 'GET 200 '[JSON] TransactionalMetrics200Response -- 'transactionalMetrics' route
    :<|> Protected :> "v1" :> "transactional" :> Capture "transactional_id" Int :> "content" :> Capture "content_id" Int :> ReqBody '[JSON] ListTransactionalVariants200ResponseContentsInner :> Verb 'PUT 200 '[JSON] UpdateTransactional200Response -- 'updateTransactional' route
    :<|> Protected :> "v1" :> "transactional" :> Capture "transactional_id" Int :> "language" :> Capture "language" Text :> ReqBody '[JSON] ListTransactionalVariants200ResponseContentsInner :> Verb 'PUT 200 '[JSON] UpdateTransactionalVariant200Response -- 'updateTransactionalVariant' route
    :<|> Protected :> "v1" :> "workspaces" :> Verb 'GET 200 '[JSON] ListWorkspaces200Response -- 'listWorkspaces' route
    :<|> Raw


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype JourneysAppClientError = JourneysAppClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for JourneysApp.
-- The backend can be used both for the client and the server. The client generated from the JourneysApp OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createJourneysAppClient@). Alternatively, provided
-- a backend, the API can be served using @runJourneysAppMiddlewareServer@.
data JourneysAppBackend a m = JourneysAppBackend
  { listActivities :: a -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Int -> m ListActivities200Response{- ^ This endpoint returns a list of \"activities\" for people, similar to your workspace's Activity Logs. This endpoint is guaranteed to return activity history within the past 30 days. It _might_ return data older than 30 days in some circumstances, but activites older than 30 days are not guaranteed. -}
  , broadcastActionLinks :: a -> Int -> Int -> Maybe Text -> Maybe Int -> Maybe Text -> m BroadcastActionLinks200Response{- ^ Returns link click metrics for an individual broadcast action. Unless you specify otherwise, the response contains data for the maximum period by days (45 days).  You cannot request fewer than 2 steps of any period (2 hours, 2 days, 2 weeks, or 2 months). For instance, `?period=days&steps=1` means two days - the 48 hours before the API request was made. `?period=days&steps=0` returns the same as the maximum of the period - `?period=days&steps=45`. See the `steps` parameter below for the maximum count of each period.  -}
  , broadcastActionMetrics :: a -> Int -> Int -> Maybe Text -> Maybe Int -> Maybe Text -> m BroadcastMetrics200Response{- ^ Returns a list of metrics for an individual action both in total and in `steps` (days, weeks, etc) over a period of time. Stepped `series` metrics return from oldest to newest (i.e. the 0-index for any result is the oldest step/period).  You cannot request fewer than 2 steps of any period (2 hours, 2 days, 2 weeks, or 2 months). For instance, `?period=days&steps=1` means two days - the 48 hours before the API request was made. `?period=days&steps=0` returns the same as the maximum of the period - `?period=days&steps=45`. See the `steps` parameter below for the maximum count of each period.  -}
  , broadcastActions :: a -> Int -> m BroadcastActions200Response{- ^ Returns the actions that occur as a part of a broadcast. -}
  , broadcastErrors :: a -> Int -> Int -> Maybe Text -> Maybe Int -> m BroadcastErrors200Response{- ^ If your broadcast produced validation errors, this endpoint can help you better understand what went wrong. Broadcast errors are generally issues in your broadcast audience and associated.  -}
  , broadcastLinks :: a -> Int -> Maybe Text -> Maybe Int -> Maybe Bool -> m BroadcastLinks200Response{- ^ Returns metrics for link clicks within a broadcast, both in total and in `series` periods (days, weeks, etc). `series` metrics are ordered oldest to newest (i.e. the 0-index for any result is the oldest step/period).  You cannot request fewer than 2 steps of any period (2 hours, 2 days, 2 weeks, or 2 months). For instance, `?period=days&steps=1` means two days - the 48 hours before the API request was made. `?period=days&steps=0` returns the same as the maximum of the period - `?period=days&steps=45`. See the `steps` parameter below for the maximum count of each period.  -}
  , broadcastMessages :: a -> Int -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> m BroadcastMessages200Response{- ^ Returns information about the deliveries (instances of messages sent to individual people) sent from an API-triggered broadcast. Provide query parameters to refine the metrics you want to return.  Use the `start_ts` and `end_ts` to find messages within a time range. If your request doesn't include `start_ts` and `end_ts` parameters, we'll return results for the 1 month period after the first trigger. If your `start_ts` and `end_ts` range is more than 12 months, we'll return 12 months of data from the most recent timestamp in your request.  -}
  , broadcastMetrics :: a -> Int -> Maybe Text -> Maybe Int -> Maybe Text -> m BroadcastMetrics200Response{- ^ Returns a list of metrics for an individual broadcast in `steps` (days, weeks, etc). We return metrics from oldest to newest (i.e. the 0-index for any result is the oldest step/period).  You cannot request fewer than 2 steps of any period (2 hours, 2 days, 2 weeks, or 2 months). For instance, `?period=days&steps=1` means two days - the 48 hours before the API request was made. `?period=days&steps=0` returns the same as the maximum of the period - `?period=days&steps=45`. See the `steps` parameter below for the maximum count of each period.  -}
  , broadcastStatus :: a -> Int -> Int -> m BroadcastStatus200Response{- ^ After triggering a broadcast you can retrieve the status of that broadcast using a GET of the `trigger_id`. You can retrieve the `trigger_id` from [Get broadcast triggers](/api/app/#operation/listBroadcastTriggers).  -}
  , getBroadcast :: a -> Int -> m GetBroadcast200Response{- ^ Returns metadata for an individual broadcast. -}
  , getBroadcastAction :: a -> Int -> Int -> m GetBroadcastAction200Response{- ^ Returns information about a specific action within a broadcast. -}
  , getBroadcastActionLanguage :: a -> Int -> Int -> Text -> m GetBroadcastAction200Response{- ^ Returns information about a translation of message in a broadcast. The message is identified by the `action_id`. -}
  , listBroadcastTriggers :: a -> Int -> m ListBroadcastTriggers200Response{- ^ Returns a list of the `triggers` for a broadcast. -}
  , listBroadcasts :: a -> m ListBroadcasts200Response{- ^ Returns a list of your API-triggered broadcasts and associated metadata. -}
  , updateBroadcastAction :: a -> Int -> Int -> BroadcastActions200ResponseActionsInner -> m GetBroadcastAction200Response{- ^ Update the contents of a broadcast action, including the body of messages or HTTP requests. -}
  , updateBroadcastActionLanguage :: a -> Int -> Int -> Text -> BroadcastActions200ResponseActionsInner -> m GetBroadcastAction200Response{- ^ Update a translation of a specific broadcast action, including the body of messages or HTTP requests. -}
  , campaignActionLinks :: a -> Int -> Int -> Maybe Text -> Maybe Int -> Maybe Text -> m CampaignActionLinks200Response{- ^ Returns link click metrics for an individual action. Unless you specify otherwise, the response contains data for the maximum period by days (45 days).  You cannot request fewer than 2 steps of any period (2 hours, 2 days, 2 weeks, or 2 months). For instance, `?period=days&steps=1` means two days - the 48 hours before the API request was made. `?period=days&steps=0` returns the same as the maximum of the period - `?period=days&steps=45`. See the `steps` parameter below for the maximum count of each period.  -}
  , campaignActionMetrics :: a -> Int -> Int -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> m BroadcastMetrics200Response{- ^ Returns a list of metrics for an individual action both in total and over a period of time (resolution). `series` metrics return from oldest to newest (i.e. the 0-index for any result is the oldest resolution).  -}
  , campaignActionMetricsDeprecated :: a -> Int -> Int -> Maybe Text -> Maybe Int -> Maybe Text -> m CampaignMetricsDeprecated200Response{- ^ Go [here](#operation/campaignActionMetrics) for our latest endpoint.  Returns a list of metrics for an individual action both in total and in `steps` (days, weeks, etc) over a period of time. Stepped `series` metrics return from oldest to newest (i.e. the 0-index for any result is the oldest step/period).  You cannot request fewer than 2 steps of any period (2 hours, 2 days, 2 weeks, or 2 months). For instance, `?period=days&steps=1` means two days - the 48 hours before the API request was made. `?period=days&steps=0` returns the same as the maximum of the period - `?period=days&steps=45`. See the `steps` parameter below for the maximum count of each period.  -}
  , campaignJourneyMetrics :: a -> Int -> Maybe Int -> Maybe Int -> Maybe Text -> m CampaignJourneyMetrics200Response{- ^ Returns a list of Journey Metrics for your campaign. These metrics show how many people triggered your campaign, were messaged, etc for the time period and \"resolution\" you set. You must provide the `start`, `end`, and `resolution` parameters or your request will return `400`.  Metrics in the response are arrays, and each index in the array corresponds to the `resolution` in your request. If you request metrics in `days`, the first result in each metric array is the first day of results and each successive increment represents another day.   Each increment represents the number of journeys that started within a time period and eventually achieved a particular metric. For example, array index 0 for the `converted` metric represents the number of journeys that started on the first day/month of results that achieved a conversion.  -}
  , campaignLinkMetrics :: a -> Int -> Maybe Text -> Maybe Int -> Maybe Bool -> m BroadcastLinks200Response{- ^ Returns metrics for link clicks within a campaign, both in total and in `series` periods (days, weeks, etc). `series` metrics are ordered oldest to newest (i.e. the 0-index for any result is the oldest step/period).  You cannot request fewer than 2 steps of any period (2 hours, 2 days, 2 weeks, or 2 months). For instance, `?period=days&steps=1` means two days - the 48 hours before the API request was made. `?period=days&steps=0` returns the same as the maximum of the period - `?period=days&steps=45`. See the `steps` parameter below for the maximum count of each period.  -}
  , campaignMetrics :: a -> Int -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> m BroadcastMetrics200Response{- ^ Returns a list of metrics for an individual campaign based on resolution and optionally, timezone, start and end times.  Use our latest endpoint with the `version` query parameter for maximum flexibility.  -}
  , campaignMetricsDeprecated :: a -> Int -> Maybe Text -> Maybe Int -> Maybe Text -> m CampaignMetricsDeprecated200Response{- ^ Go [here](#operation/campaignMetrics) for our latest endpoint.  Returns a list of metrics for an individual campaign in `steps` (days, weeks, etc). We return metrics from oldest to newest (i.e. the 0-index for any result is the oldest step/period).   You cannot request fewer than 2 steps of any period (2 hours, 2 days, 2 weeks, or 2 months). For instance, `?period=days&steps=1` means two days - the 48 hours before the API request was made. `?period=days&steps=0` returns the same as the maximum of the period - `?period=days&steps=45`. See the `steps` parameter below for the maximum count of each period.  -}
  , getCampaignAction :: a -> Int -> Int -> m GetCampaignAction200Response{- ^ Returns information about a specific action in a campaign. -}
  , getCampaignActionTranslation :: a -> Int -> Int -> Text -> m GetCampaignAction200Response{- ^ Returns a translated version of a message in a campaign. The message is identified by the `action_id`. -}
  , getCampaignMessages :: a -> Int -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Int -> Maybe Int -> m GetCampaignMessages200Response{- ^ Returns information about the deliveries (instances of messages sent to individual people) sent from a campaign. Provide query parameters to refine the metrics you want to return. Use the `start_ts` and `end_ts` to find messages within a time range. If your request doesn't include `start_ts` and `end_ts` parameters, we'll return the most recent 6 months of messages. If your `start_ts` and `end_ts` range is more than 12 months, we'll return 12 months of data from the most recent timestamp in your request. -}
  , getCampaigns :: a -> Int -> m GetCampaigns200Response{- ^ Returns metadata for an individual campaign. -}
  , listCampaignActions :: a -> Int -> Maybe Text -> m ListCampaignActions200Response{- ^ Returns the operations in a campaign workflow. Each object in the response represents an action or 'tile' in the campaign builder.  This endpoint returns up to 10 `actions` at a time. If there is another page of results, the response will include a `next` string. Pass this string as the `start` parameter to get the next page of results.  -}
  , listCampaigns :: a -> m ListCampaigns200Response{- ^ Returns a list of your campaigns and associated metadata. -}
  , updateCampaignAction :: a -> Int -> Int -> ListCampaignActions200ResponseActionsInner -> m GetCampaignAction200Response{- ^ Update the contents of a campaign action, including the body of messages and HTTP requests. -}
  , updateCampaignActionTranslation :: a -> Int -> Int -> Text -> ListCampaignActions200ResponseActionsInner -> m GetCampaignAction200Response{- ^ Update the contents of a language variant of a campaign action, including the body of the messages and HTTP requests. -}
  , addCollection :: a -> AddCollectionRequest -> m AddCollection200Response{- ^ Create a new collection and provide the `data` that you'll access from the collection or the `url` that you'll download CSV or JSON data from.  **Note**: A collection cannot be more than 10 MB in size. No individual row in the collection can be more than 10 KB.  -}
  , deleteCollection :: a -> Int -> m NoContent{- ^ Remove a collection and associated contents. Before you delete a collection, make sure that you aren't referencing it in active campaign messages or broadcasts; references to a deleted collection will appear empty and may prevent your messages from making sense to your audience. -}
  , getCollection :: a -> Int -> m AddCollection200Response{- ^ Retrieves details about a collection, including the `schema` and `name`. This request does not include the `content` of the collection (the values associated with keys in the schema). -}
  , getCollectionContents :: a -> Int -> m ((Map.Map String Value)){- ^ Retrieve the contents of a collection (the `data` from when you created or updated a collection). Each `row` in the collection is represented as a JSON blob in the response. -}
  , getCollections :: a -> m GetCollections200Response{- ^ Returns a list of all of your collections, including the `name` and `schema` for each collection. -}
  , updateCollection :: a -> Int -> UpdateCollectionRequest -> m AddCollection200Response{- ^ Update the `name` or replace the contents of a collection. Updating the `data` or `url` for your collection fully replaces the contents of the collection.  **Note**:  * If you reference your collection by name in active campaign messages, changing the name of the collection will cause references to the previous name to return an empty data set. * A collection cannot be more than 10 MB in size. No individual row in the collection can be more than 10 KB.  -}
  , updateCollectionContents :: a -> Int -> (Map.Map String Value) -> m AddCollection200Response{- ^ Replace the contents of a collection (the `data` from when you created or updated a collection). The request is a free-form object containing the keys you want to reference from the collection and the corresponding values. This request replaces the current contents of the collection entirely.  If you don't want to update the contents directly—you want to change the `name` or data `url` for your collection, use the [update a collection](#operation/updateCollection) endpoint.  **Note**: A collection cannot be more than 10 MB in size. No individual row in the collection can be more than 10 KB.  -}
  , getPeopleById :: a -> GetPeopleByIdRequest -> m GetPeopleById200Response{- ^ Return attributes and devices for up to 100 customers by ID. If an ID in the request does not exist, the response omits it. -}
  , getPeopleEmail :: a -> Maybe Text -> m GetPeopleEmail200Response{- ^ Return a list of people in your workspace matching an email address.  -}
  , getPeopleFilter :: a -> Maybe Text -> Maybe Int -> GetPeopleFilterRequest -> m GetPeopleFilter200Response{- ^ Provide a filter to search for people in your workspace. Your filter can filter people by segment (using the Segment ID) and attribute values; when you filter by attributes, you can use `eq` (matching an attribute value) or `exists` (matching when a person has the attribute). Use the `and` array, `or` array, and `not` object to create a complex filter. The `not` selector is an object that takes a single filter.  Returns arrays of `identifiers` and `ids`. In general, you should rely on the newer `identifiers` array, which contains more complete information about each person captured by the filter in your request, than the `ids` array, which only contains `id` values.  You can return up to 1000 people per request. If you want to return a larger set of people in a single request, you may want to use the [`/exports`](#tag/Exports) API instead.   -}
  , getPersonActivities :: a -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> m GetPersonActivities200Response{- ^ Return a list of activities performed by, or for, a customer. Activities are things like attribute changes and message sends.   This endpoint is guaranteed to return activity history within the past 30 days. It might return data older than 30 days in some circumstances, but activites older than 30 days are not guaranteed.  -}
  , getPersonAttributes :: a -> Text -> Maybe Text -> m GetPersonAttributes200Response{- ^ Return a list of attributes for a customer profile. You can use attributes to fashion segments or as liquid merge fields in your messages. -}
  , getPersonMessages :: a -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> m GetPersonMessages200Response{- ^ Returns information about the deliveries sent to a person. Provide query parameters to refine the data you want to return.  Use the `start_ts` and `end_ts` to find messages within a time range. If your request doesn't include `start_ts` and `end_ts` parameters, we'll return the most recent 6 months of messages. If your `start_ts` and `end_ts` range is more than 6 months, we'll return 6 months of data from the most recent timestamp in your request.  -}
  , getPersonRelationships :: a -> Text -> Maybe Text -> Maybe Int -> m GetPersonRelationships200Response{- ^ Return a list of objects that a person is related to.  You can use the `start` parameter with the `next` property in responses to return pages of results. However, it's possible that you'll see duplicate entries across pages. If you want to export objects or relationships, you may want to use the export feature in our UI to return complete results.  -}
  , getPersonSegments :: a -> Text -> Maybe Text -> m GetPersonSegments200Response{- ^ Returns a list of segments that a customer profile belongs to. -}
  , getPersonSubscriptionPreferences :: a -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> m GetPersonSubscriptionPreferences200Response{- ^ Returns a list of subscription preferences for a person, including the custom header of the subscription preferences page, topic names, and topic descriptions. Returns translated data when you send a language in the query. -}
  , deleteSuppression :: a -> Text -> Text -> m NoContent{- ^ Remove an address from the ESP's suppression list. -}
  , getSuppression :: a -> Text -> m GetSuppression200Response{- ^ Look up an email address to learn if, and why, it was suppressed by the email service provider (ESP). -}
  , getSuppressionByType :: a -> Text -> Maybe Int -> Maybe Int -> m GetSuppression200Response{- ^ Find addresses suppressed by the Email Service Provider (ESP) for a particular reason—bounces, blocks, spam reports, or invalid email addresses.  You can get up to 1000 addresses per request. Use the `offset` parameter to get addresses beyond the first 1000.  -}
  , postSuppression :: a -> Text -> Text -> m Value{- ^ Suppress an email address at the email service provider (ESP). Addresses suppressed this way are only suppressed through the ESP; these adresses are _not_ suppressed in Customer.io, so the person can remain in your workspace (though emails to the address would be blocked at the ESP). -}
  , getCioAllowlist :: m GetCioAllowlist200Response{- ^ Returns a list of IP addresses that you need to allowlist if you're using a firewall or [Custom SMTP](/use-your-smtp-server) provider's IP access management settings to deny access to unknown IP addresses.  These addresses apply to all message types and webhooks, except push notifications.  -}
  , getArchivedMessage :: a -> Text -> m GetArchivedMessage200Response{- ^ Returns the archived copy of a delivery, including the message body, recipient, and metrics. This endpoint is limited to 100 requests per day. -}
  , getMessage :: a -> Text -> m GetMessage200Response{- ^ Return a information about, and metrics for, a delivery—the instance of a message intended for an individual recipient person. -}
  , listMessages :: a -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> m ListMessages200Response{- ^ Return a list of deliveries, including metrics for each delivery, for messages in your workspace. The request body contains filters determining the deliveries you want to return information about.  Use the `start_ts` and `end_ts` parameters to find messages within a time range. We limit your requests to 6 months. If your request doesn't include `start_ts` and `end_ts` parameters, we'll return the most recent 6 months of deliveries. If `start_ts` is greater than 6-months before `end_ts`, we only send back 6 months of data. If only `end_ts` is specified, we return 6 months of data before this timestamp. If only `start_ts` is specified, we then set the `end_ts` to the current time and deliver 6 months of data prior to this timestamp.  -}
  , deletetNewsletters :: a -> Int -> m NoContent{- ^ Deletes an individual newsletter, including content, settings, and metrics. It will be removed from segments, and its templates will no longer show in the Message Library.  If the newsletter is an in-app message, this cancels any undelivered, in-app message, too.  -}
  , getNewsletterLinks :: a -> Int -> Maybe Text -> Maybe Int -> Maybe Bool -> m BroadcastLinks200Response{- ^ Returns metrics for link clicks within a newsletter, both in total and in `series` periods (days, weeks, etc). `series` metrics are ordered oldest to newest (i.e. the 0-index for any result is the oldest step/period).  You cannot request fewer than 2 steps of any period (2 hours, 2 days, 2 weeks, or 2 months). For instance, `?period=days&steps=1` means two days - the 48 hours before the API request was made. `?period=days&steps=0` returns the same as the maximum of the period - `?period=days&steps=45`. See the `steps` parameter below for the maximum count of each period.  -}
  , getNewsletterMetrics :: a -> Int -> Maybe Text -> Maybe Int -> Maybe Text -> m BroadcastMetrics200Response{- ^ Returns a list of metrics for an individual newsletter in `steps` (days, weeks, etc). We return metrics from oldest to newest (i.e. the 0-index for any result is the oldest step/period).  You cannot request fewer than 2 steps of any period (2 hours, 2 days, 2 weeks, or 2 months). For instance, `?period=days&steps=1` means two days - the 48 hours before the API request was made. `?period=days&steps=0` returns the same as the maximum of the period - `?period=days&steps=45`. See the `steps` parameter below for the maximum count of each period.  -}
  , getNewsletterMsgMeta :: a -> Int -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Int -> Maybe Int -> m GetNewsletterMsgMeta200Response{- ^ Returns information about the deliveries (instances of messages sent to individual people) sent from a newsletter. Provide query parameters to refine the metrics you want to return. Use the `start_ts` and `end_ts` to find messages within a time range. If your request doesn't include `start_ts` and `end_ts` parameters, we'll return up to 6 months of results beginning with the first delivery generated from the newsletter. If your `start_ts` and `end_ts` range is more than 12 months, we'll return 12 months of data from the most recent timestamp in your request. -}
  , getNewsletterTestGroups :: a -> Int -> m GetNewsletterTestGroups200Response{- ^ Returns information about each test group in a newsletter, including content ids for each group. -}
  , getNewsletterVariant :: a -> Int -> Int -> m GetNewsletterVariant200Response{- ^ Returns information about a specific variant of a newsletter, where a variant is either a language in a multi-language newsletter or a part of an A/B test. -}
  , getNewsletterVariantTranslation :: a -> Int -> Text -> m GetNewsletterVariant200Response{- ^ Returns information about a specific language variant of a newsletter. If your newsletter includes A/B tests, use [Get a translation in a newsletter test group](/api/app/#operation/getNewsletterVariantTranslationTest). -}
  , getNewsletterVariantTranslationTest :: a -> Int -> Text -> Text -> m GetNewsletterVariant200Response{- ^ Returns information about a specific language variant of a newsletter in an A/B test group. You can retrieve `test_group_ids` from [Get variants in a newsletter test group](/api/app/#operation/getNewsletterVariantTest). -}
  , getNewsletters :: a -> Int -> m GetNewsletters200Response{- ^ Returns metadata for an individual newsletter. -}
  , getVariantLinks :: a -> Int -> Int -> Maybe Text -> Maybe Int -> Maybe Text -> m GetVariantLinks200Response{- ^ Returns link click metrics for an individual newsletter variant—an individual language in a multi-language newsletter or a message in an A/B test. Unless you specify otherwise, the response contains data for the maximum period by days (45 days).  You cannot request fewer than 2 steps of any period (2 hours, 2 days, 2 weeks, or 2 months). For instance, `?period=days&steps=1` means two days - the 48 hours before the API request was made. `?period=days&steps=0` returns the same as the maximum of the period - `?period=days&steps=45`. See the `steps` parameter below for the maximum count of each period.  -}
  , getVariantMetrics :: a -> Int -> Int -> Maybe Text -> Maybe Int -> Maybe Text -> m BroadcastMetrics200Response{- ^ Returns a metrics for an individual newsletter variant—either an individual language in a multi-language newsletter or a message in an A/B test. This endpoint returns metrics both in total and in `steps` (days, weeks, etc) over a `period` of time. Stepped `series` metrics are arranged from oldest to newest (i.e. the 0-index for any result is the oldest period/step).  You cannot request fewer than 2 steps of any period (2 hours, 2 days, 2 weeks, or 2 months). For instance, `?period=days&steps=1` means two days - the 48 hours before the API request was made. `?period=days&steps=0` returns the same as the maximum of the period - `?period=days&steps=45`. See the `steps` parameter below for the maximum count of each period.  -}
  , listNewsletterVariants :: a -> Int -> m ListNewsletterVariants200Response{- ^ Returns the content variants of a newsletter—these are either different languages in a multi-language newsletter or A/B tests. -}
  , listNewsletters :: a -> Maybe Int -> Maybe Text -> Maybe Text -> m ListNewsletters200Response{- ^ Returns a list of your newsletters and associated metadata. -}
  , updateNewsletterTestTranslation :: a -> Int -> Text -> Text -> ListNewsletterVariants200ResponseContentsInner -> m GetNewsletterVariant200Response{- ^ Update the translation of a newsletter variant in an A/B test. You can retrieve a list of `test_group_ids` from [Get variants in a newsletter test group](/api/app/#operation/getNewsletterVariantTest). -}
  , updateNewsletterVariant :: a -> Int -> Int -> ListNewsletterVariants200ResponseContentsInner -> m GetNewsletterVariant200Response{- ^ Update the contents of a newsletter variant (a specific language of your message or a part of an A/B test), including the body of a newsletter. -}
  , updateNewsletterVariantTranslation :: a -> Int -> Text -> ListNewsletterVariants200ResponseContentsInner -> m GetNewsletterVariant200Response{- ^ Update the translation of a newsletter variant. If your newsletter includes A/B tests, use [Update a translation in a newsletter test group](/api/app/#operation/updateNewsletterTestTranslation). -}
  , getObjectAttributes :: a -> Int -> Text -> Maybe Text -> m GetObjectAttributes200Response{- ^ Get a list of attributes for an object. Attributes are things you know about an object—like an account name, billing date, etc.  -}
  , getObjectRelationships :: a -> Int -> Text -> Maybe Text -> Maybe Int -> Maybe Text -> m GetObjectRelationships200Response{- ^ Get a list of people people related to an object.  You can use the `start` parameter with the `next` property in responses to return pages of results. However, it's possible that you'll see duplicate entries across pages. If you want to export objects or relationships, you may want to use the export feature in our UI to return complete results.  -}
  , getObjectTypes :: a -> m GetObjectTypes200Response{- ^ Returns a list of object types in your system. Because each object type is an incrementing ID, you may need to use this endpoint to find the ID of the object type you want to query, create, or modify. -}
  , getObjectsFilter :: a -> Maybe Text -> Maybe Int -> GetObjectsFilterRequest -> m GetObjectsFilter200Response{- ^ Use a set of filter conditions to find objects in your workspace. Returns a list of object IDs that you can use to look up object attributes, or to create or modify objects.  The list is paged if you have a large number of objects. You can set the `limit` for the number of objects returned, and use the `start` to page through the results. It's possible that you'll see duplicate entries across pages. If you want to export objects or relationships, you may want to use the export feature in our UI to return complete results.  -}
  , createWebhook :: a -> ListWebhooks200ResponseReportingWebhooksInner -> m ListWebhooks200ResponseReportingWebhooksInner{- ^ Create a new webhook configuration. -}
  , deleteWebhook :: a -> Int -> m NoContent{- ^ Delete a reporting webhook's configuration. -}
  , getWebhook :: a -> Int -> m ListWebhooks200ResponseReportingWebhooksInner{- ^ Returns information about a specific reporting webhook. -}
  , listWebhooks :: a -> m ListWebhooks200Response{- ^ Return a list of all of your reporting webhooks -}
  , updateWebhook :: a -> Int -> ListWebhooks200ResponseReportingWebhooksInner -> m ListWebhooks200ResponseReportingWebhooksInner{- ^ Update the configuration of a reporting webhook. Turn events on or off, change the webhook URL, etc. -}
  , createManSegment :: a -> CreateManSegmentRequest -> m CreateManSegment200Response{- ^ Create a manual segment with a name and a description. This request creates an empty segment. -}
  , deleteManSegment :: a -> Int -> m NoContent{- ^ Delete a manual segment. -}
  , getSegment :: a -> Int -> m CreateManSegment200Response{- ^ Return information about a segment. -}
  , getSegmentCount :: a -> Int -> m GetSegmentCount200Response{- ^ Returns the membership count for a segment. -}
  , getSegmentDependencies :: a -> Int -> m GetSegmentDependencies200Response{- ^ Use this endpoint to find out which campaigns and newsletters use a segment. -}
  , getSegmentMembership :: a -> Int -> Maybe Text -> Maybe Int -> m GetSegmentMembership200Response{- ^ Returns customers in a segment. This endpoint returns an array of `identifiers`; each object in the array represents a person and contains the identifier values allowed in your workspace. In general, we recommend that you use `identifiers` rather than `ids` to find people, because it provides more information.    **If your workspace does not use email as a unique identifier** for people, `identifiers` does not contain `email` values. Go to your [Workspace Settings](/workspaces/#migrate-workspace) to find out which identifiers your workspace supports.  The `ids` array only lists ID values for people in a segment; if your workspace uses both `email` and `id` as identifiers, it's possible that a member of your segment does not have an `id` value, resulting in an empty string in the `ids` array.  -}
  , listSegments :: a -> m ListSegments200Response{- ^ Retrieve a list of all of your segments. -}
  , sendEmail :: a -> SendEmailRequest -> m SendEmail200Response{- ^ Send a transactional email. You can send a message using a `transactional_message_id` or send your own `body`, `subject`, and `from` values at send time. The `transactional_message_id` can be either the numerical ID for the template or the *Trigger Name* that you assigned the template.  If you want to send your own `body`, `subject`, and `from` values to populate your message at send time, we recommend that you pass a `transactional_message_id` anyway; the values you pass in the request will override the template.   You can find your `transactional_message_id` from the code sample in the **Overview** tab for your transactional message in the user interface, or you can look up a list of your transactional messages through the [App API](#tag/Transactional).  Customer.io attributes metrics to a `transactional_message_id`; if you don't provide a `transactional_message_id`, we attribute metrics to `\"transactional_message_id\": 1`. You can create empty transactional messages in the UI and override the `body`, `subject`, and `from` values at send time. This provides flexibility in your integration and lets you organize metrics (rather than gathering metrics for all of your transactional messages against a single ID).  -}
  , sendPush :: a -> SendPushRequest -> m SendEmail200Response{- ^ Send a transactional push. You send a message using a `transactional_message_id` for a transactional push message template composed in the user interface. You can optionally override any of the template values at send time. The `transactional_message_id` can be either the numerical ID for the template or the *Trigger Name* that you assigned the template.  You can find your `transactional_message_id` from the code sample in the **Overview** tab for your transactional message in the user interface, or you can look up a list of your transactional messages through the [App API](#tag/Transactional).  -}
  , triggerBroadcast :: a -> Int -> TriggerBroadcastRequest -> m TriggerBroadcast200Response{- ^ Manually trigger a broadcast, and provide data to populate messages in your trigger. The shape of the request changes based on the type of audience you broadcast to: a segment, a list of emails, a list of customer IDs, a map of users, or a data file. You can reference properties in the `data` object from this request using liquid—`{{trigger.<property_in_data_obj>}}`.  If your broadcast produces a `422` error, you can [get more information about the errors](#operation/broadcastErrors) to see what went wrong.  **This endpoint is rate-limited to one request every 10 seconds.** After exceeding this, you'll receive a status of 429. Broadcasts are optimized to send messages to a large audience and not for one-to-one interactions. Use our [transactional API](#send-email) or event-triggered campaigns to respond to your audience on an individual, one-to-one basis.  -}
  , getSender :: a -> Int -> m GetSender200Response{- ^ Returns information about a specific sender. -}
  , getSenderUsage :: a -> Int -> m GetSenderUsage200Response{- ^ Returns lists of the campaigns and newsletters that use a sender. -}
  , listSenders :: a -> Maybe Text -> Maybe Int -> Maybe Text -> m ListSenders200Response{- ^ Returns a list of senders in your workspace. Senders are who your messages are \"from\". -}
  , deleteSnippet :: a -> Text -> m NoContent{- ^ Remove a snippet. You can only remove a snippet that is not in use. If your snippet is in use, you'll receive a `400` error. -}
  , listSnippets :: a -> m ListSnippets200Response{- ^ Returns a list of snippets in your workspace. Snippets are pieces of reusable content, like a common footer for your emails. -}
  , updateSnippets :: a -> ListSnippets200ResponseSnippetsInner -> m UpdateSnippets200Response{- ^ In your payload, you'll pass a `name` and `value`. Snippet names are unique. If the snippet `name` does not exist, we'll create a new snippet. If the `name` exists, we'll update the existing snippet.   -}
  , getTopics :: a -> m GetTopics200Response{- ^ Returns a list of subscription topics in your workspace. If there are no topics, it returns an empty array. -}
  , getTransactional :: a -> Int -> m GetTransactional200Response{- ^ Returns information about an individual transactional message. -}
  , getTransactionalVariant :: a -> Int -> Text -> m GetTransactionalVariant200Response{- ^ Returns information about a translation of an individual transactional message, including the message content. -}
  , listTransactional :: a -> m ListTransactional200Response{- ^ Returns a list of your transactional messages—the transactional IDs that you use to trigger an individual transactional delivery. This endpoint does not return information about deliveries (instances of a message sent to a person) themselves. -}
  , listTransactionalVariants :: a -> Int -> m ListTransactionalVariants200Response{- ^ Returns the content variants of a transactional message, where each variant represents a different language. -}
  , transactionalLinks :: a -> Int -> Maybe Text -> Maybe Int -> Maybe Bool -> m BroadcastLinks200Response{- ^ Returns metrics for clicked links from a transactional message, both in total and in `series` periods (days, weeks, etc). `series` metrics are ordered oldest to newest (i.e. the 0-index for any result is the oldest step/period).  You cannot request fewer than 2 steps of any period (2 hours, 2 days, 2 weeks, or 2 months). For instance, `?period=days&steps=1` means two days - the 48 hours before the API request was made. `?period=days&steps=0` returns the same as the maximum of the period - `?period=days&steps=45`. See the `steps` parameter below for the maximum count of each period.  -}
  , transactionalMessages :: a -> Int -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> m TransactionalMessages200Response{- ^ Returns information about the deliveries (instances of messages sent to individual people) from a transactional message. Provide query parameters to refine the metrics you want to return.  Use the `start_ts` and `end_ts` to find messages within a time range. If your request doesn't include `start_ts` and `end_ts` parameters, we'll return the most recent 6 months of messages. If your `start_ts` and `end_ts` range is more than 12 months, we'll return 12 months of data from the most recent timestamp in your request.  -}
  , transactionalMetrics :: a -> Int -> Maybe Text -> Maybe Int -> m TransactionalMetrics200Response{- ^ Returns a list of metrics for a transactional message in `steps` (days, weeks, etc). We return metrics from oldest to newest (i.e. the 0-index for any result is the oldest step/period).  You cannot request fewer than 2 steps of any period (2 hours, 2 days, 2 weeks, or 2 months). For instance, `?period=days&steps=1` means two days - the 48 hours before the API request was made. `?period=days&steps=0` returns the same as the maximum of the period - `?period=days&steps=45`. See the `steps` parameter below for the maximum count of each period.  -}
  , updateTransactional :: a -> Int -> Int -> ListTransactionalVariants200ResponseContentsInner -> m UpdateTransactional200Response{- ^ Update the body of a transactional email. This fully overwrites your existing transactional message. We'll use your updated content for any future transactional requests (`/v1/send/email`), so make sure that you test your message before you update it.   -}
  , updateTransactionalVariant :: a -> Int -> Text -> ListTransactionalVariants200ResponseContentsInner -> m UpdateTransactionalVariant200Response{- ^ Update the body and other data of a specific language variant for a transactional message. This fully overwrites this specific translation of your existing transactional message.   -}
  , listWorkspaces :: a -> m ListWorkspaces200Response{- ^ Returns a list of workspaces in your account. -}
  }

-- | Authentication settings for JourneysApp.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data JourneysAppAuth = JourneysAppAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }

newtype JourneysAppClient a = JourneysAppClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative JourneysAppClient where
  pure x = JourneysAppClient (\_ -> pure x)
  (JourneysAppClient f) <*> (JourneysAppClient x) =
    JourneysAppClient (\env -> f env <*> x env)

instance Monad JourneysAppClient where
  (JourneysAppClient a) >>= f =
    JourneysAppClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO JourneysAppClient where
  liftIO io = JourneysAppClient (\_ -> liftIO io)

createJourneysAppClient :: JourneysAppBackend AuthClient JourneysAppClient
createJourneysAppClient = JourneysAppBackend{..}
  where
    ((coerce -> listActivities) :<|>
     (coerce -> broadcastActionLinks) :<|>
     (coerce -> broadcastActionMetrics) :<|>
     (coerce -> broadcastActions) :<|>
     (coerce -> broadcastErrors) :<|>
     (coerce -> broadcastLinks) :<|>
     (coerce -> broadcastMessages) :<|>
     (coerce -> broadcastMetrics) :<|>
     (coerce -> broadcastStatus) :<|>
     (coerce -> getBroadcast) :<|>
     (coerce -> getBroadcastAction) :<|>
     (coerce -> getBroadcastActionLanguage) :<|>
     (coerce -> listBroadcastTriggers) :<|>
     (coerce -> listBroadcasts) :<|>
     (coerce -> updateBroadcastAction) :<|>
     (coerce -> updateBroadcastActionLanguage) :<|>
     (coerce -> campaignActionLinks) :<|>
     (coerce -> campaignActionMetrics) :<|>
     (coerce -> campaignActionMetricsDeprecated) :<|>
     (coerce -> campaignJourneyMetrics) :<|>
     (coerce -> campaignLinkMetrics) :<|>
     (coerce -> campaignMetrics) :<|>
     (coerce -> campaignMetricsDeprecated) :<|>
     (coerce -> getCampaignAction) :<|>
     (coerce -> getCampaignActionTranslation) :<|>
     (coerce -> getCampaignMessages) :<|>
     (coerce -> getCampaigns) :<|>
     (coerce -> listCampaignActions) :<|>
     (coerce -> listCampaigns) :<|>
     (coerce -> updateCampaignAction) :<|>
     (coerce -> updateCampaignActionTranslation) :<|>
     (coerce -> addCollection) :<|>
     (coerce -> deleteCollection) :<|>
     (coerce -> getCollection) :<|>
     (coerce -> getCollectionContents) :<|>
     (coerce -> getCollections) :<|>
     (coerce -> updateCollection) :<|>
     (coerce -> updateCollectionContents) :<|>
     (coerce -> getPeopleById) :<|>
     (coerce -> getPeopleEmail) :<|>
     (coerce -> getPeopleFilter) :<|>
     (coerce -> getPersonActivities) :<|>
     (coerce -> getPersonAttributes) :<|>
     (coerce -> getPersonMessages) :<|>
     (coerce -> getPersonRelationships) :<|>
     (coerce -> getPersonSegments) :<|>
     (coerce -> getPersonSubscriptionPreferences) :<|>
     (coerce -> deleteSuppression) :<|>
     (coerce -> getSuppression) :<|>
     (coerce -> getSuppressionByType) :<|>
     (coerce -> postSuppression) :<|>
     (coerce -> getCioAllowlist) :<|>
     (coerce -> getArchivedMessage) :<|>
     (coerce -> getMessage) :<|>
     (coerce -> listMessages) :<|>
     (coerce -> deletetNewsletters) :<|>
     (coerce -> getNewsletterLinks) :<|>
     (coerce -> getNewsletterMetrics) :<|>
     (coerce -> getNewsletterMsgMeta) :<|>
     (coerce -> getNewsletterTestGroups) :<|>
     (coerce -> getNewsletterVariant) :<|>
     (coerce -> getNewsletterVariantTranslation) :<|>
     (coerce -> getNewsletterVariantTranslationTest) :<|>
     (coerce -> getNewsletters) :<|>
     (coerce -> getVariantLinks) :<|>
     (coerce -> getVariantMetrics) :<|>
     (coerce -> listNewsletterVariants) :<|>
     (coerce -> listNewsletters) :<|>
     (coerce -> updateNewsletterTestTranslation) :<|>
     (coerce -> updateNewsletterVariant) :<|>
     (coerce -> updateNewsletterVariantTranslation) :<|>
     (coerce -> getObjectAttributes) :<|>
     (coerce -> getObjectRelationships) :<|>
     (coerce -> getObjectTypes) :<|>
     (coerce -> getObjectsFilter) :<|>
     (coerce -> createWebhook) :<|>
     (coerce -> deleteWebhook) :<|>
     (coerce -> getWebhook) :<|>
     (coerce -> listWebhooks) :<|>
     (coerce -> updateWebhook) :<|>
     (coerce -> createManSegment) :<|>
     (coerce -> deleteManSegment) :<|>
     (coerce -> getSegment) :<|>
     (coerce -> getSegmentCount) :<|>
     (coerce -> getSegmentDependencies) :<|>
     (coerce -> getSegmentMembership) :<|>
     (coerce -> listSegments) :<|>
     (coerce -> sendEmail) :<|>
     (coerce -> sendPush) :<|>
     (coerce -> triggerBroadcast) :<|>
     (coerce -> getSender) :<|>
     (coerce -> getSenderUsage) :<|>
     (coerce -> listSenders) :<|>
     (coerce -> deleteSnippet) :<|>
     (coerce -> listSnippets) :<|>
     (coerce -> updateSnippets) :<|>
     (coerce -> getTopics) :<|>
     (coerce -> getTransactional) :<|>
     (coerce -> getTransactionalVariant) :<|>
     (coerce -> listTransactional) :<|>
     (coerce -> listTransactionalVariants) :<|>
     (coerce -> transactionalLinks) :<|>
     (coerce -> transactionalMessages) :<|>
     (coerce -> transactionalMetrics) :<|>
     (coerce -> updateTransactional) :<|>
     (coerce -> updateTransactionalVariant) :<|>
     (coerce -> listWorkspaces) :<|>
     _) = client (Proxy :: Proxy JourneysAppAPI)

-- | Run requests in the JourneysAppClient monad.
runJourneysAppClient :: Config -> JourneysAppClient a -> ExceptT ClientError IO a
runJourneysAppClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runJourneysAppClientWithManager manager clientConfig cl

-- | Run requests in the JourneysAppClient monad using a custom manager.
runJourneysAppClientWithManager :: Manager -> Config -> JourneysAppClient a -> ExceptT ClientError IO a
runJourneysAppClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a JourneysAppClientError
callJourneysApp
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> JourneysAppClient a -> m a
callJourneysApp env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (JourneysAppClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the JourneysApp server at the provided host and port.
runJourneysAppServer
  :: (MonadIO m, MonadThrow m)
  => Config -> JourneysAppAuth -> JourneysAppBackend AuthServer (ExceptT ServerError IO) -> m ()
runJourneysAppServer config auth backend = runJourneysAppMiddlewareServer config requestMiddlewareId auth backend

-- | Run the JourneysApp server at the provided host and port.
runJourneysAppMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> JourneysAppAuth -> JourneysAppBackend AuthServer (ExceptT ServerError IO) -> m ()
runJourneysAppMiddlewareServer Config{..} middleware auth backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationJourneysApp auth backend

-- | Plain "Network.Wai" Application for the JourneysApp server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationJourneysApp :: JourneysAppAuth -> JourneysAppBackend AuthServer (ExceptT ServerError IO) -> Application
serverWaiApplicationJourneysApp auth backend = serveWithContextT (Proxy :: Proxy JourneysAppAPI) context id (serverFromBackend backend)
  where
    context = serverContext auth
    serverFromBackend JourneysAppBackend{..} =
      (coerce listActivities :<|>
       coerce broadcastActionLinks :<|>
       coerce broadcastActionMetrics :<|>
       coerce broadcastActions :<|>
       coerce broadcastErrors :<|>
       coerce broadcastLinks :<|>
       coerce broadcastMessages :<|>
       coerce broadcastMetrics :<|>
       coerce broadcastStatus :<|>
       coerce getBroadcast :<|>
       coerce getBroadcastAction :<|>
       coerce getBroadcastActionLanguage :<|>
       coerce listBroadcastTriggers :<|>
       coerce listBroadcasts :<|>
       coerce updateBroadcastAction :<|>
       coerce updateBroadcastActionLanguage :<|>
       coerce campaignActionLinks :<|>
       coerce campaignActionMetrics :<|>
       coerce campaignActionMetricsDeprecated :<|>
       coerce campaignJourneyMetrics :<|>
       coerce campaignLinkMetrics :<|>
       coerce campaignMetrics :<|>
       coerce campaignMetricsDeprecated :<|>
       coerce getCampaignAction :<|>
       coerce getCampaignActionTranslation :<|>
       coerce getCampaignMessages :<|>
       coerce getCampaigns :<|>
       coerce listCampaignActions :<|>
       coerce listCampaigns :<|>
       coerce updateCampaignAction :<|>
       coerce updateCampaignActionTranslation :<|>
       coerce addCollection :<|>
       coerce deleteCollection :<|>
       coerce getCollection :<|>
       coerce getCollectionContents :<|>
       coerce getCollections :<|>
       coerce updateCollection :<|>
       coerce updateCollectionContents :<|>
       coerce getPeopleById :<|>
       coerce getPeopleEmail :<|>
       coerce getPeopleFilter :<|>
       coerce getPersonActivities :<|>
       coerce getPersonAttributes :<|>
       coerce getPersonMessages :<|>
       coerce getPersonRelationships :<|>
       coerce getPersonSegments :<|>
       coerce getPersonSubscriptionPreferences :<|>
       coerce deleteSuppression :<|>
       coerce getSuppression :<|>
       coerce getSuppressionByType :<|>
       coerce postSuppression :<|>
       coerce getCioAllowlist :<|>
       coerce getArchivedMessage :<|>
       coerce getMessage :<|>
       coerce listMessages :<|>
       coerce deletetNewsletters :<|>
       coerce getNewsletterLinks :<|>
       coerce getNewsletterMetrics :<|>
       coerce getNewsletterMsgMeta :<|>
       coerce getNewsletterTestGroups :<|>
       coerce getNewsletterVariant :<|>
       coerce getNewsletterVariantTranslation :<|>
       coerce getNewsletterVariantTranslationTest :<|>
       coerce getNewsletters :<|>
       coerce getVariantLinks :<|>
       coerce getVariantMetrics :<|>
       coerce listNewsletterVariants :<|>
       coerce listNewsletters :<|>
       coerce updateNewsletterTestTranslation :<|>
       coerce updateNewsletterVariant :<|>
       coerce updateNewsletterVariantTranslation :<|>
       coerce getObjectAttributes :<|>
       coerce getObjectRelationships :<|>
       coerce getObjectTypes :<|>
       coerce getObjectsFilter :<|>
       coerce createWebhook :<|>
       coerce deleteWebhook :<|>
       coerce getWebhook :<|>
       coerce listWebhooks :<|>
       coerce updateWebhook :<|>
       coerce createManSegment :<|>
       coerce deleteManSegment :<|>
       coerce getSegment :<|>
       coerce getSegmentCount :<|>
       coerce getSegmentDependencies :<|>
       coerce getSegmentMembership :<|>
       coerce listSegments :<|>
       coerce sendEmail :<|>
       coerce sendPush :<|>
       coerce triggerBroadcast :<|>
       coerce getSender :<|>
       coerce getSenderUsage :<|>
       coerce listSenders :<|>
       coerce deleteSnippet :<|>
       coerce listSnippets :<|>
       coerce updateSnippets :<|>
       coerce getTopics :<|>
       coerce getTransactional :<|>
       coerce getTransactionalVariant :<|>
       coerce listTransactional :<|>
       coerce listTransactionalVariants :<|>
       coerce transactionalLinks :<|>
       coerce transactionalMessages :<|>
       coerce transactionalMetrics :<|>
       coerce updateTransactional :<|>
       coerce updateTransactionalVariant :<|>
       coerce listWorkspaces :<|>
       serveDirectoryFileServer "static")

-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: JourneysAppAuth -> AuthHandler Request AuthServer
authHandler JourneysAppAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Authorization" (requestHeaders req) of
      Just header -> case extractBearerAuth header of
        Just key -> lookupUser key
        Nothing -> throwError (authError req)
      Nothing -> throwError (authError req)

type Protected = AuthProtect "bearer"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest ("Bearer " <> key) (addHeader "Authorization")

serverContext :: JourneysAppAuth -> Context (AuthHandler Request AuthServer ': '[])
serverContext auth = authHandler auth :. EmptyContext
