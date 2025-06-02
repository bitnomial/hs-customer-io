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

module JourneysTrack.API
  ( -- * Client and Server
    Config(..)
  , JourneysTrackBackend(..)
  , createJourneysTrackClient
  , runJourneysTrackServer
  , runJourneysTrackMiddlewareServer
  , runJourneysTrackClient
  , runJourneysTrackClientWithManager
  , callJourneysTrack
  , JourneysTrackClient
  , JourneysTrackClientError(..)
  -- ** Servant
  , JourneysTrackAPI
  -- ** Plain WAI Application
  , serverWaiApplicationJourneysTrack
  -- ** Authentication
  , JourneysTrackAuth(..)
  , clientAuth
  , Protected
  ) where

import           JourneysTrack.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import qualified Data.Aeson                         as Aeson
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
import           Network.Wai.Middleware.HttpAuth    (extractBasicAuth)
import           Servant                            (ServerError, serveWithContextT, throwError)
import           Servant.API                        hiding (addHeader)
import           Servant.API.BasicAuth              (BasicAuthData (..))
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.API.Experimental.Auth      (AuthProtect)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost, basicAuthReq, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest)
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


-- | Servant type-level API, generated from the OpenAPI spec for JourneysTrack.
type JourneysTrackAPI
    =    Protected :> "api" :> "v1" :> "forms" :> Capture "form_id" Text :> "submit" :> ReqBody '[JSON] SubmitFormRequest :> Verb 'POST 204 '[JSON] NoContent -- 'submitForm' route
    :<|> Protected :> "api" :> "v1" :> "customers" :> Capture "identifier" IdentifyIdentifierParameter :> "devices" :> ReqBody '[JSON] AddDeviceRequest :> Verb 'PUT 200 '[JSON] NoContent -- 'addDevice' route
    :<|> Protected :> "api" :> "v1" :> "customers" :> Capture "identifier" IdentifyIdentifierParameter :> Verb 'DELETE 200 '[JSON] NoContent -- 'delete' route
    :<|> Protected :> "api" :> "v1" :> "customers" :> Capture "identifier" IdentifyIdentifierParameter :> "devices" :> Capture "device_id" Text :> Verb 'DELETE 200 '[JSON] NoContent -- 'deleteDevice' route
    :<|> Protected :> "api" :> "v1" :> "customers" :> Capture "identifier" IdentifyIdentifierParameter :> ReqBody '[JSON] IdentifyRequest :> Verb 'PUT 200 '[JSON] NoContent -- 'identify' route
    :<|> Protected :> "api" :> "v1" :> "merge_customers" :> ReqBody '[JSON] MergeRequest :> Verb 'POST 200 '[JSON] NoContent -- 'merge' route
    :<|> Protected :> "api" :> "v1" :> "customers" :> Capture "identifier" IdentifyIdentifierParameter :> "suppress" :> Verb 'POST 200 '[JSON] NoContent -- 'suppress' route
    :<|> "unsubscribe" :> Capture "delivery_id" Text :> ReqBody '[JSON] UnsubscribeRequest :> Verb 'POST 200 '[JSON] NoContent -- 'unsubscribe' route
    :<|> Protected :> "api" :> "v1" :> "customers" :> Capture "identifier" IdentifyIdentifierParameter :> "unsuppress" :> Verb 'POST 200 '[JSON] NoContent -- 'unsuppress' route
    :<|> "api" :> "v1" :> "metrics" :> ReqBody '[JSON] MetricsRequest :> Verb 'POST 200 '[JSON] NoContent -- 'metrics' route
    :<|> "api" :> "v1" :> "push" :> "events" :> ReqBody '[JSON] PushMetricsRequest :> Verb 'POST 200 '[JSON] NoContent -- 'pushMetrics' route
    :<|> Protected :> "api" :> "v1" :> "customers" :> Capture "identifier" TrackIdentifierParameter :> "events" :> ReqBody '[JSON] TrackRequest :> Verb 'POST 200 '[JSON] NoContent -- 'track' route
    :<|> Protected :> "api" :> "v1" :> "events" :> ReqBody '[JSON] TrackAnonymousRequest :> Verb 'POST 200 '[JSON] NoContent -- 'trackAnonymous' route
    :<|> Protected :> "api" :> "v1" :> "accounts" :> "region" :> Verb 'GET 200 '[JSON] GetRegion200Response -- 'getRegion' route
    :<|> Protected :> "api" :> "v1" :> "segments" :> Capture "segment_id" Int :> "add_customers" :> QueryParam "id_type" Text :> ReqBody '[JSON] AddToSegmentRequest :> Verb 'POST 200 '[JSON] NoContent -- 'addToSegment' route
    :<|> Protected :> "api" :> "v1" :> "segments" :> Capture "segment_id" Int :> "remove_customers" :> QueryParam "id_type" Text :> ReqBody '[JSON] RemoveFromSegmentRequest :> Verb 'POST 200 '[JSON] NoContent -- 'removeFromSegment' route
    :<|> Protected :> "api" :> "v2" :> "batch" :> ReqBody '[JSON] BatchRequest :> Verb 'POST 200 '[JSON] NoContent -- 'batch' route
    :<|> Protected :> "api" :> "v2" :> "entity" :> ReqBody '[JSON] EntityRequest :> Verb 'POST 200 '[JSON] NoContent -- 'entity' route
    :<|> Raw


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype JourneysTrackClientError = JourneysTrackClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for JourneysTrack.
-- The backend can be used both for the client and the server. The client generated from the JourneysTrack OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createJourneysTrackClient@). Alternatively, provided
-- a backend, the API can be served using @runJourneysTrackMiddlewareServer@.
data JourneysTrackBackend a m = JourneysTrackBackend
  { submitForm :: a -> Text -> SubmitFormRequest -> m NoContent{- ^ Submit a form response. If Customer.io does not recognize the `form_id` we create a new form connection (found on the *Data & Integrations* > *Integrations* > *Forms* page). Form submissions with the same ID are treated as submissions from the same form.  The `data` object _must_ contain at least one of `id` or `email` (depending on the identifiers supported in your workspace)—or a field that is mapped to one of these identifiers—to identify the form respondent. If the person who submitted the form does not already exist, we create them (like an [identify](#operation/identify) request).  Additional keys in the `data` object represent form fields and values from the form that a person submitted. By default, we map form fields in your request directly to attributes, e.g. if you have a form field called `first_name`, we map that field to the `first_name` attribute.  **NOTES**:    * You cannot disable fields that you send to this API. If you send a field (as `data`) to this API, we'll include it in the form submission.   * If an identifier in your form is called something like `email_address` rather than `email` in your initial request, you'll receive a `400`, but we'll still add your form on the **Data & Integrations** > **Integrations** > **Forms** page. You can then re-map your `email_address` field to `email`, and your form will begin working normally.   * Customer.io reserves `form_id`, `form_name`, `form_type`, `form_url`, and `form_url_param` keys. If your request includes these keys, Customer.io ignores them.  -}
  , addDevice :: a -> IdentifyIdentifierParameter -> AddDeviceRequest -> m NoContent{- ^ Customers can have more than one device. Use this method to add iOS and Android devices to, or update devices for, a customer profile. -}
  , delete :: a -> IdentifyIdentifierParameter -> m NoContent{- ^ Deleting a customer removes them, and all of their information, from Customer.io.  **NOTE**: Calls that update customers by ID can also create a customer. If you send data to Customer.io through other means (like the Javascript snippet), after you delete a customer, you may accidentally recreate the customer. You cannot delete a customer using the Javascript snippet alone.  -}
  , deleteDevice :: a -> IdentifyIdentifierParameter -> Text -> m NoContent{- ^ Remove a device from a customer profile. If you continue sending data about a device to Customer.io, you may inadvertently re-add the device to the customer profile. -}
  , identify :: a -> IdentifyIdentifierParameter -> IdentifyRequest -> m NoContent{- ^ Adds or updates a person.  If your request does _not_ include `cio_id` and the identifiers in the request body do not belong to a person, your request adds a person.  If a person already exists with the identifier in the request path, your request updates that person. If the identifier in the path does not belong to a person but you use an identifier in your request body that _does_ belong to a person, your request updates the person and assigns them the identifier in the path.  If the identifier in the path and request body belong to different people, your request may return `200 OK` but produce an *Attribute Update Failure* for the identifier in the payload.  If you want to update a person's identifiers after they are set, you must reference them using their `cio_id` in the format `cio_<cio_id_value>`—unless when updating an `email` with the [Allow updates to email using ID](/workspaces/#update-email-with-id) setting enabled. You can get the `cio_id` value from the [App API](/api/#tag/Customers). If your request includes a `cio_id`, we'll attempt to update that person, including any identifiers in the request. If the `cio_id` does not exist or belongs to a person who was deleted, we'll drop the request.  For workspaces using `email` as an identifier, `email` is case-insensitive. The addresses `person@example.com` and `PERSON@example.com` would represent the same person.  -}
  , merge :: a -> MergeRequest -> m NoContent{- ^ Merge two customer profiles together. The payload contains `primary` and `secondary` profile objects. The primary profile remains after the merge and the secondary is deleted. This operation is _not_ reversible.   The following information is merged into the primary profile from the secondary profile: * Attributes that are not set, or are empty, on the primary. * The most recent 30-days of event history. Events merged from the secondary person cannot trigger campaigns. * Manual segments that the primary person did not already belong to. * Message delivery history.  * Campaign journeys that the primary person has not entered. If the secondary person has started a journey that the primary person has not, the primary person continues on that campaign journey after the merge. If the secondary person has completed journeys that the primary person has not, the primary person gains these historical journeys after the merge. This may be important for determining entry (or re-entry) criteria for subsequent campaigns, segments, etc.  -}
  , suppress :: a -> IdentifyIdentifierParameter -> m NoContent{- ^ Delete a customer profile and prevent the person's identifier(s) from being re-added to your workspace. Any future API calls or operations referencing the specified ID are ignored. If you suppress a person in a workspace set that identifies people by *email or ID* and both identifiers are set, both the person's email and ID are suppressed.  <div class=\"fly-panel bg-warning\"> <div class=\"fly-panel-body\"> <p class=\"callout-head text--bold text-warning mrg-t-none\"><svg class=\"api-icon\"><path fill-rule=\"evenodd\" clip-rule=\"evenodd\" d=\"M15.4127 13.3333L9.18133 1.43333C8.95116 0.994094 8.49623 0.718884 8.00033 0.718884C7.50444 0.718884 7.04951 0.994094 6.81933 1.43333L0.587332 13.3333C0.370821 13.7467 0.386147 14.2431 0.627743 14.6423C0.869339 15.0415 1.30205 15.2854 1.76867 15.2853H14.2313C14.698 15.2854 15.1307 15.0415 15.3723 14.6423C15.6139 14.2431 15.6292 13.7467 15.4127 13.3333ZM7.33333 5.61533C7.33333 5.24714 7.63181 4.94867 8 4.94867C8.36819 4.94867 8.66667 5.24714 8.66667 5.61533V9.61533C8.66667 9.98352 8.36819 10.282 8 10.282C7.63181 10.282 7.33333 9.98352 7.33333 9.61533V5.61533ZM8.01466 13.2887H8.03333C8.29806 13.2844 8.54988 13.1735 8.73182 12.9812C8.91376 12.7888 9.01044 12.5312 9 12.2667C8.97854 11.7209 8.53019 11.2893 7.984 11.2887H7.96533C7.70125 11.2935 7.4502 11.4043 7.26865 11.5961C7.0871 11.788 6.99029 12.0447 7 12.3087C7.02073 12.8546 7.46838 13.2869 8.01466 13.2887Z\" /></svg>&nbsp;This API permanently deletes people</p> <div class=\"text-warning\"><p>Suppressing a person way deletes their profile <i>and</i> suppresses the identifier you reference in the path of this call, preventing you from re-adding a person using the same identifier (until you unsuppress the identifier). You cannot recover a profile after you suppress it. In general, should use this API sparingly—for GDPR/CCPA requests, etc. </p> <p>If you want to keep a record of a person but prevent them from receiving messages, you should set the person's unsubscribed attribute (or use other attributes to represent complex subscription preferences) instead.</p></div> </div> </div>  -}
  , unsubscribe :: Text -> UnsubscribeRequest -> m NoContent{- ^ This endpoint lets you set a global unsubscribed status outside of the subscription pathways native to Customer.io. If you use [custom unsubscribe links](/multiple-subscription-types), you can host a custom unsubscribe page and use this API to send unsubscribe data, associated with a particular delivery, to Customer.io.  **NOTE**: This endpoint **requires** a `Content-type: application/json` header. This endpoint **does not require** an `Authorization` header.  Your request sets a person's `unsubscribed` attribute to `true`, attributes their unsubscribe request to the individual email/delivery that they unsubscribed from, and lets you segment your audience based on `email_unsubscribed` events when you use a custom subscription center.  If you use a custom subscription center (managing subscriptions to various types of messages with custom attributes), this request *does not* set a custom attribute. You must perform a [separate request](#operation/identify) to update a person's custom subscription attributes.  -}
  , unsuppress :: a -> IdentifyIdentifierParameter -> m NoContent{- ^ Unsuppressing a profile allows you to add the customer back to Customer.io. If you unsuppress a person in a workspace set that identifies people by *email or ID* and the suppressed person had both an email and ID, both the person's email and ID are unsuppressed.  Unsuppressing a profile does not recreate the profile that you previously suppressed. Rather, it just makes the identifier available again. Identifying a person after unsuppressing them creates a new profile, with none of the history of the previously suppressed identifier.  -}
  , metrics :: MetricsRequest -> m NoContent{- ^ This endpoint helps you report metrics from channels that aren't native to Customer.io or don't rely on our SDKs. When we deliver a message, we include a CIO-Delivery-ID header. This is the `delivery_id` in the payload. You can use it as a UTL and you can pass it as a UTM parameter in links, etc to track metrics when people click, convert, etc.  -}
  , pushMetrics :: PushMetricsRequest -> m NoContent{- ^ While this endpoint still works, you should take advantage of our [universal metrics endpoint](#operation/metrics). It supports channels besides push and lets you provide additional information with some metrics.  Use this endpoint to report device-side push metrics—opened, converted, and delivered—back to Customer.io, so you can track the effectiveness of your push notifications. Customer.io has no way of knowing about these metrics, or associating metrics with a specific message, unless you report them back to us.  When Customer.io delivers a push notification, we include `CIO-Delivery-ID` and `CIO-Delivery-Token` parameters. Reference these in your payload as the `delivery_id` and `device_id` respectively with the type of device-side `event` metric that you want to associate with your push notification and the person represented by the `device_id`.   -}
  , track :: a -> TrackIdentifierParameter -> TrackRequest -> m NoContent{- ^ Send an event associated with a person, referenced by the identifier in the path. There are three defined event `type` values: `page`, `screen` and `event`. Page and screen events represent website page views and mobile app screen views respectively; the `name` for these event types is intended to be the page or screen a person visited or viewed. Any other event, is given the `event` type.  We automatically trim leading and trailing spaces from event names.  **Reserved Properties**  There are a few important values which, if sent with the events that trigger campaigns, will override your campaign settings:  * `from_address` * `recipient` * `reply_to`  When using the Javascript snippet to track events, you must call the Behavioral Tracking API call after identifying the customer or the event will not associate with the customer’s profile.  -}
  , trackAnonymous :: a -> TrackAnonymousRequest -> m NoContent{- ^ An anonymous event represents a person you haven't identified yet. When you identify a person, you can set their `anonymous_id` attribute. If [event merging](/anonymous-events/#turn-on-merging) is turned on in your workspace, and the attribute matches the `anonymous_id` in one or more events that were logged within the last 30 days, we associate those events with the person. If you associate an event with a person within 72 hours of the timestamp on the event, you can trigger campaigns from the event.  There are three possible event `type` values: `page`, `screen` and `event`. Page and screen events represent website page views and mobile app screen views respectively; the `name` for these event types is intended to be the page or screen a person visited or viewed. Any other event, is given the `event` type.  **Note**: Avoid using names with leading or trailing spaces, because you can't reference event names with leading or trailing spaces in campaigns, etc. In workspaces created after September 21, 2021, we trim leading and trailing spaces from event names automatically to fix this issue.  -}
  , getRegion :: a -> m GetRegion200Response{- ^ This endpoint returns the appropriate region and URL for your Track API credentials. Use it to determine the URLs you should use to successfully complete other requests.  You can perform this operation against either of the track API regional URLs; it returns your region in either case.   This endpoint also returns an `environment_id`, which represents the workspace the credentials are valid for.   -}
  , addToSegment :: a -> Int -> Maybe Text -> AddToSegmentRequest -> m NoContent{- ^ Add people to a manual segment by ID. You are limited to 1000 customer IDs per request.  This endpoint lets you add people to manual segments, but a segment must exist before you can add people to it. You can create and find manual segments using the [App API](/api/#operation/createManSegment).  When you call this API, you can pass an `id_type` query parameter determining the type of identifier you want to use, `id`, `email`, or `cio_id`; If you don't pass this parameter, it defaults to `id`. The request body always uses `ids`, even though it'll accept either IDs, emails, or `cio_id`s depending on the query parameter. Everybody in the payload must use the same kind of identifier; we'll ignore values in the `ids` array that don't match the `id_type` parameter.  **NOTE**: You cannot add people to data-driven segments using the API. See [our documentation on segments](/segments) for more information about segments.  -}
  , removeFromSegment :: a -> Int -> Maybe Text -> RemoveFromSegmentRequest -> m NoContent{- ^ You can remove users from a manual segment by ID. You are limited to 1000 customer IDs per request.  This endpoint requires people to have `id` attributes. If your workspace does not use `id` as an identifier, or you have not assigned people `id` values, you cannot remove people from manual segments using the API. Our user interface does not have this limitation. You can remove people from manual segments through the UI as a part of a campaign workflow.  **NOTE**: You cannot remove people from data-driven segments using the API. See [our documentation on segments](/segments) for more information about segments.  -}
  , batch :: a -> BatchRequest -> m NoContent{- ^ This endpoint lets you batch requests for different people and objects in a single request. Each object in your array represents an individual \"entity\" operation—it represents a change for a person, an object, or a delivery.   You can mix types in this request; you are not limited to a batch containing only objects or only people. An \"object\" is a non-person entity that you want to associate with one or more people—like a company, an educational course that people enroll in, etc.  Your batch request must be smaller than 500kb. Each of the requests within the batch must also be 32kb or smaller.  -}
  , entity :: a -> EntityRequest -> m NoContent{- ^ This endpoint lets you create, update, or delete a single person or object—including managing relationships between objects and people.   An \"object\" is any kind of non-person entity that you want to associate with one or more people—like a company, an educational course that people signed up for, a product, etc.   Your request must be smaller than 32kb.   -}
  }

-- | Authentication settings for JourneysTrack.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data JourneysTrackAuth = JourneysTrackAuth
  { lookupUser :: BasicAuthData -> Handler AuthServer
  , authError :: Request -> ServerError
  }

newtype JourneysTrackClient a = JourneysTrackClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative JourneysTrackClient where
  pure x = JourneysTrackClient (\_ -> pure x)
  (JourneysTrackClient f) <*> (JourneysTrackClient x) =
    JourneysTrackClient (\env -> f env <*> x env)

instance Monad JourneysTrackClient where
  (JourneysTrackClient a) >>= f =
    JourneysTrackClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO JourneysTrackClient where
  liftIO io = JourneysTrackClient (\_ -> liftIO io)

createJourneysTrackClient :: JourneysTrackBackend AuthClient JourneysTrackClient
createJourneysTrackClient = JourneysTrackBackend{..}
  where
    ((coerce -> submitForm) :<|>
     (coerce -> addDevice) :<|>
     (coerce -> delete) :<|>
     (coerce -> deleteDevice) :<|>
     (coerce -> identify) :<|>
     (coerce -> merge) :<|>
     (coerce -> suppress) :<|>
     (coerce -> unsubscribe) :<|>
     (coerce -> unsuppress) :<|>
     (coerce -> metrics) :<|>
     (coerce -> pushMetrics) :<|>
     (coerce -> track) :<|>
     (coerce -> trackAnonymous) :<|>
     (coerce -> getRegion) :<|>
     (coerce -> addToSegment) :<|>
     (coerce -> removeFromSegment) :<|>
     (coerce -> batch) :<|>
     (coerce -> entity) :<|>
     _) = client (Proxy :: Proxy JourneysTrackAPI)

-- | Run requests in the JourneysTrackClient monad.
runJourneysTrackClient :: Config -> JourneysTrackClient a -> ExceptT ClientError IO a
runJourneysTrackClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runJourneysTrackClientWithManager manager clientConfig cl

-- | Run requests in the JourneysTrackClient monad using a custom manager.
runJourneysTrackClientWithManager :: Manager -> Config -> JourneysTrackClient a -> ExceptT ClientError IO a
runJourneysTrackClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a JourneysTrackClientError
callJourneysTrack
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> JourneysTrackClient a -> m a
callJourneysTrack env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (JourneysTrackClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the JourneysTrack server at the provided host and port.
runJourneysTrackServer
  :: (MonadIO m, MonadThrow m)
  => Config -> JourneysTrackAuth -> JourneysTrackBackend AuthServer (ExceptT ServerError IO) -> m ()
runJourneysTrackServer config auth backend = runJourneysTrackMiddlewareServer config requestMiddlewareId auth backend

-- | Run the JourneysTrack server at the provided host and port.
runJourneysTrackMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> JourneysTrackAuth -> JourneysTrackBackend AuthServer (ExceptT ServerError IO) -> m ()
runJourneysTrackMiddlewareServer Config{..} middleware auth backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationJourneysTrack auth backend

-- | Plain "Network.Wai" Application for the JourneysTrack server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationJourneysTrack :: JourneysTrackAuth -> JourneysTrackBackend AuthServer (ExceptT ServerError IO) -> Application
serverWaiApplicationJourneysTrack auth backend = serveWithContextT (Proxy :: Proxy JourneysTrackAPI) context id (serverFromBackend backend)
  where
    context = serverContext auth
    serverFromBackend JourneysTrackBackend{..} =
      (coerce submitForm :<|>
       coerce addDevice :<|>
       coerce delete :<|>
       coerce deleteDevice :<|>
       coerce identify :<|>
       coerce merge :<|>
       coerce suppress :<|>
       coerce unsubscribe :<|>
       coerce unsuppress :<|>
       coerce metrics :<|>
       coerce pushMetrics :<|>
       coerce track :<|>
       coerce trackAnonymous :<|>
       coerce getRegion :<|>
       coerce addToSegment :<|>
       coerce removeFromSegment :<|>
       coerce batch :<|>
       coerce entity :<|>
       serveDirectoryFileServer "static")

-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: JourneysTrackAuth -> AuthHandler Request AuthServer
authHandler JourneysTrackAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Authorization" (requestHeaders req) of
      Just header -> case extractBasicAuth header of
        Just (user, password) -> lookupUser (BasicAuthData user password)
        Nothing -> throwError (authError req)
      Nothing -> throwError (authError req)

type Protected = AuthProtect "basic"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = BasicAuthData

clientAuth :: BasicAuthData -> AuthClient
clientAuth key = mkAuthenticatedRequest key basicAuthReq

serverContext :: JourneysTrackAuth -> Context (AuthHandler Request AuthServer ': '[])
serverContext auth = authHandler auth :. EmptyContext
