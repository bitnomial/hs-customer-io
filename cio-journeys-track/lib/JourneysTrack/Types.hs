{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module JourneysTrack.Types (
  APNS (..),
  APNSCIO (..),
  APNSCIOPush (..),
  ActionObject (..),
  ActionWidget (..),
  ActivityObject (..),
  ActivityObjectCustomerIdentifiers (..),
  ActivityObjectData (..),
  ActivityTypes (..),
  AddDevice (..),
  AddDevice400Response (..),
  AddDevice400ResponseMeta (..),
  AddDeviceAllOfDevice (..),
  AddDeviceRequest (..),
  AddDeviceRequestDevice (..),
  AddDeviceRequestDeviceAllOfAttributes (..),
  AddRelationships (..),
  AddRelationships1 (..),
  AddToSegmentRequest (..),
  AllAndroidProps (..),
  And (..),
  And1 (..),
  And2 (..),
  And3 (..),
  AndAudienceFilter (..),
  AndAudienceFilterAndInner (..),
  AnonymousEventsRequest (..),
  ApnsWithSdk (..),
  ApnsWithoutSdk (..),
  ApsBasicPush (..),
  ApsBasicPushAps (..),
  ApsBasicPushApsAlert (..),
  ArchivedMessageObject (..),
  Attachments (..),
  Attribute (..),
  Attribute1 (..),
  AttributeAudienceFilter (..),
  AttributeChangeAction (..),
  AttributeChangeValue (..),
  Audience (..),
  AudienceFilter (..),
  Batch207Response (..),
  Batch207ResponseErrorsInner (..),
  BatchRequest (..),
  BatchRequestBatchInner (..),
  BlockWidget (..),
  BroadcastActionObject (..),
  BroadcastObject (..),
  BroadcastObjectActionsInner (..),
  BroadcastTriggerObject (..),
  CIOObjectID (..),
  CIOObjectIDUpdatesOnly (..),
  CampaignObject (..),
  CampaignRequest (..),
  CioId1 (..),
  CioId2 (..),
  CioId3 (..),
  CioId4 (..),
  CioSubscriptionPreferences (..),
  CollectionResponse (..),
  CommonTriggerProps (..),
  ComplexAlert (..),
  ComplexAudienceFilter (..),
  ComplexAudienceFilterAndInner (..),
  ComplexAudienceFilterNot (..),
  ConditionalWidget (..),
  ContentObject (..),
  CriticalAlert (..),
  CustomAndroidProps (..),
  CustomIntegration (..),
  CustomRecipients (..),
  CustomerAttributesObject (..),
  CustomerAttributesObjectCustomer (..),
  CustomerAttributesObjectCustomerAttributes (..),
  CustomerAttributesObjectCustomerTimestamps (..),
  CustomerEvents (..),
  CustomerIdentifiers (..),
  DataFileURL (..),
  DataObjectOnly (..),
  DataObjectOnlyAndroid (..),
  DataObjectOnlyData (..),
  DataToProcess (..),
  Date (..),
  DefaultAudience (..),
  Delete (..),
  Delete1 (..),
  DeleteDevice (..),
  DeleteDeviceAllOfDevice (..),
  DeleteRelationships (..),
  DeleteRelationships1 (..),
  Delivery (..),
  DeliveryAttributes (..),
  DeliveryIdentifiers (..),
  DeliveryOperations (..),
  DeviceObject (..),
  DeviceObjectCdp (..),
  DeviceObjectCdpCommon (..),
  DeviceObjectCdpCommonAttributes (..),
  DeviceObjectCommon (..),
  Email (..),
  Email1 (..),
  Email2 (..),
  Email3 (..),
  EmailEvents (..),
  EmailMessage (..),
  EmailMessage1 (..),
  Emails (..),
  Entity400Response (..),
  Entity400ResponseErrorsInner (..),
  EntityRequest (..),
  EspSuppression (..),
  EspSuppressionSuppressionsInner (..),
  Event (..),
  EventsRequest (..),
  ExportObject (..),
  ExportSharedProps (..),
  ExportType (..),
  FCM (..),
  FCMMessage (..),
  FCMMessageApns (..),
  FCMMessageApnsPayload (..),
  FCMMessageApnsPayloadAps (..),
  FCMMessageApnsPayloadApsAlert (..),
  FCMMessageApnsPayloadApsSound (..),
  FCMMessageApnsPayloadCIO (..),
  FCMMessageApnsPayloadCIOPush (..),
  FcmAndroid (..),
  FcmAndroidWithSdk (..),
  FcmAndroidWithSdkMessage (..),
  FcmAndroidWithoutSdk (..),
  FcmBasicPush (..),
  FcmBasicPushMessage (..),
  FcmBasicPushMessageData (..),
  FcmBasicPushMessageNotification (..),
  FcmIosWithSdk (..),
  FcmIosWithoutSdk (..),
  FcmIosWithoutSdkMessage (..),
  FcmIosWithoutSdkMessageApns (..),
  FcmIosWithoutSdkMessageApnsPayload (..),
  FixedGridWidget (..),
  FixedHorizontalListWidget (..),
  FixedHorizontalScrollWidget (..),
  FixedListWidget (..),
  Form (..),
  GetRegion200Response (..),
  ID (..),
  IDs (..),
  IconWidget (..),
  Id (..),
  Id1 (..),
  Id2 (..),
  Identify (..),
  Identify1 (..),
  Identify1AllOfCioRelationshipsInner (..),
  Identify1AllOfCioRelationshipsInnerIdentifiers (..),
  Identify1AllOfIdentifiers (..),
  IdentifyAllOfAttributes (..),
  IdentifyAllOfAttributesCioSubscriptionPreferences (..),
  IdentifyAllOfIdentifiers (..),
  IdentifyAnonymous (..),
  IdentifyAnonymousAllOfCioRelationshipsInner (..),
  IdentifyAnonymousAllOfCioRelationshipsInnerIdentifiers (..),
  IdentifyByEmail (..),
  IdentifyById (..),
  IdentifyIdentifierParameter (..),
  IdentifyPerson (..),
  IdentifyPersonAllOfAttributes (..),
  IdentifyPersonAllOfAttributesCioSubscriptionPreferences (..),
  IdentifyPersonAllOfCioRelationships (..),
  IdentifyPersonAllOfIdentifiers (..),
  IdentifyRequest (..),
  IdentifyRequestCioRelationships (..),
  IdentifyRequestCioRelationshipsRelationshipsInner (..),
  IdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers (..),
  IdentifyRequestCioSubscriptionPreferences (..),
  IdentifyRequestValue (..),
  Ids (..),
  ImageWidget (..),
  ImportIdentifier (..),
  ImportObject (..),
  ImportRequest (..),
  ImportState (..),
  ImportType (..),
  InApp (..),
  InAppMessageActionEvent (..),
  InAppMessageActionEventDetail (..),
  InAppMessageDismissedEvent (..),
  InAppMessageErrorEvent (..),
  InAppMessageEventDetail (..),
  InAppMessageOpenedEvent (..),
  InAppMessageOpenedEventDetail (..),
  IosFcmAndApns (..),
  IosSharedOptions (..),
  LinkMetrics (..),
  LinkMetricsLink (..),
  LinkMetricsMetric (..),
  LinkMetricsMetricSeries (..),
  Merge400Response (..),
  Merge400ResponseMeta (..),
  MergeRequest (..),
  MergeRequestPrimary (..),
  MergeRequestSecondary (..),
  MessageDelivery (..),
  MessageMetrics (..),
  MessageObject (..),
  MessageObjectMetrics (..),
  MessageTotalMetrics (..),
  MessageType (..),
  Metric (..),
  MetricsRequest (..),
  MobileScreenView (..),
  MobileScreenView1 (..),
  MsgTemplateIdsInner (..),
  NewsletterObject (..),
  Not (..),
  Not1 (..),
  NotAudienceFilter (..),
  NotificationAndDataObject (..),
  NotificationAndDataObjectNotification (..),
  Object (..),
  Object1 (..),
  Object2 (..),
  Object2Audience (..),
  ObjectAddRelationships (..),
  ObjectAddRelationshipsAllOfCioRelationships (..),
  ObjectAddRelationshipsAllOfIdentifiers (..),
  ObjectAttribute (..),
  ObjectAttribute1 (..),
  ObjectCommon (..),
  ObjectCommonAllOfIdentifiers (..),
  ObjectCommonIdentify (..),
  ObjectDelete (..),
  ObjectDeleteRelationships (..),
  ObjectDeleteRelationshipsAllOfCioRelationships (..),
  ObjectDeleteRelationshipsAllOfIdentifiers (..),
  ObjectFilter (..),
  ObjectFilterAnd (..),
  ObjectFilterAndAndInner (..),
  ObjectFilterNot (..),
  ObjectFilterNotNot (..),
  ObjectFilterOr (..),
  ObjectID (..),
  ObjectIDCreateAndUpdate (..),
  ObjectIdentifiers (..),
  ObjectIdentify (..),
  ObjectIdentifyAllOfCioRelationships (..),
  ObjectIdentifyAllOfIdentifiers (..),
  ObjectIdentifyAllOfIdentifiers1 (..),
  ObjectIdentifyAnonymous (..),
  ObjectIdentifyAnonymousAllOfCioRelationships (..),
  ObjectIdentifyAnonymousAllOfIdentifiers (..),
  ObjectOperations (..),
  ObjectattributeFilter (..),
  Or (..),
  Or1 (..),
  Or2 (..),
  Or3 (..),
  OrAudienceFilter (..),
  OrAudienceFilterOrInner (..),
  PageView (..),
  PageView1 (..),
  People (..),
  PeopleFilter (..),
  PeriodMessageMetrics (..),
  PeriodMessageMetricsDeprecated (..),
  PeriodWebhookMetrics (..),
  PeriodWebhookMetricsDeprecated (..),
  Person (..),
  Person1 (..),
  PersonAddDevice (..),
  PersonAddDeviceAllOfDevice (..),
  PersonAddDeviceAllOfIdentifiers (..),
  PersonAddRelationships (..),
  PersonAddRelationshipsAllOfCioRelationships (..),
  PersonAddRelationshipsAllOfIdentifiers (..),
  PersonAttributes (..),
  PersonCommon (..),
  PersonDelete (..),
  PersonDeleteAllOfIdentifiers (..),
  PersonDeleteDevice (..),
  PersonDeleteDeviceAllOfIdentifiers (..),
  PersonDeleteRelationships (..),
  PersonDeleteRelationshipsAllOfCioRelationships (..),
  PersonDeleteRelationshipsAllOfIdentifiers (..),
  PersonEvent (..),
  PersonEventAllOfIdentifiers (..),
  PersonMerge (..),
  PersonOneOf (..),
  PersonOneOf1 (..),
  PersonOneOf2 (..),
  PersonOneOf3 (..),
  PersonOneOf3Primary (..),
  PersonOneOf3Secondary (..),
  PersonOneOfAllOfAttributes (..),
  PersonOperations (..),
  PersonPage (..),
  PersonPageAllOfIdentifiers (..),
  PersonScreen (..),
  PersonScreenAllOfIdentifiers (..),
  PersonSuppress (..),
  PersonSuppressAllOfIdentifiers (..),
  PersonUnsuppress (..),
  PersonUnsuppressAllOfIdentifiers (..),
  Preprocessor (..),
  Push (..),
  PushEvents (..),
  PushMetricsRequest (..),
  Relationship (..),
  RelationshipAudience (..),
  RemoveFromSegmentRequest (..),
  ReportingWebhook (..),
  RequestMethod (..),
  SDKIntegration (..),
  SDKIntegrationMessage (..),
  SMS (..),
  SMSMMS (..),
  Segment (..),
  Segment1 (..),
  SegmentActionsInner (..),
  SegmentAudienceFilter (..),
  SegmentMsgTemplatesInner (..),
  SegmentResponseObject (..),
  Sendemail (..),
  SenderIdentityObject (..),
  SendingState (..),
  Sendpush (..),
  SendpushAllOfCustomDevice (..),
  SendpushAllOfCustomPayload (..),
  SendpushAllOfCustomPayloadIos (..),
  SendpushAllOfIdentifiers (..),
  SendpushAllOfTransactionalMessageId (..),
  SimpleAudienceFilter (..),
  Slack (..),
  SlackEvents (..),
  SmsActionObject (..),
  SmsEvents (..),
  Snippet (..),
  StandardAnonymousEvent (..),
  StandardAnonymousEventData (..),
  StandardEvent (..),
  StandardEventData (..),
  State (..),
  SubmitFormRequest (..),
  SubmitFormRequestData (..),
  Suppress (..),
  TextWidget (..),
  TrackAnonymousRequest (..),
  TrackIdentifierParameter (..),
  TrackMetrics (..),
  TrackRequest (..),
  TransactionalActionObject (..),
  TransactionalObject (..),
  TransactionalSharedEmailObject (..),
  TransactionalSharedEmailObjectAttachments (..),
  TransactionalSharedObject (..),
  TransactionalSharedObjectIdentifiers (..),
  TransactionalSharedPushObject (..),
  TransactionalSharedPushObjectCustomDevice (..),
  TransactionalSharedPushObjectCustomPayload (..),
  TransactionalSharedPushObjectCustomPayloadAndroid (..),
  TransactionalSharedPushObjectCustomPayloadAndroidMessage (..),
  TransactionalSharedPushObjectCustomPayloadAndroidMessageAndroid (..),
  TransactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotification (..),
  TransactionalSharedPushObjectCustomPayloadAndroidMessageNotification (..),
  TransactionalSharedPushObjectCustomPayloadIos (..),
  UnsubscribeRequest (..),
  Unsuppress (..),
  UserMaps (..),
  UserMapsAllOfPerUserDataInner (..),
  V1CioRelationships (..),
  V1CioRelationshipsRelationshipsInner (..),
  Webhook (..),
  Webhook1 (..),
  Webhook2 (..),
  Webhook3 (..),
  WebhookEvents (..),
  WebhookMetrics (..),
  WidgetCrossAxisAlignment (..),
  WidgetMainAxisAlignment (..),
  WithTemplate (..),
  WithoutTemplate (..),
  Workspace (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)


-- | 
data APNS = APNS
  { aPNSCIO :: Maybe APNSCIO -- ^ 
  , aPNSAps :: Maybe FCMMessageApnsPayloadAps -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON APNS where
  parseJSON = genericParseJSON optionsAPNS
instance ToJSON APNS where
  toJSON = genericToJSON optionsAPNS

optionsAPNS :: Options
optionsAPNS =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("aPNSCIO", "CIO")
      , ("aPNSAps", "aps")
      ]


-- | Contains options supported by the Customer.io SDK.
data APNSCIO = APNSCIO
  { aPNSCIOPush :: APNSCIOPush -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON APNSCIO where
  parseJSON = genericParseJSON optionsAPNSCIO
instance ToJSON APNSCIO where
  toJSON = genericToJSON optionsAPNSCIO

optionsAPNSCIO :: Options
optionsAPNSCIO =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("aPNSCIOPush", "push")
      ]


-- | Describes push notification options supported by the CIO SDK.
data APNSCIOPush = APNSCIOPush
  { aPNSCIOPushLink :: Maybe Text -- ^ A deep link (to a page in your app), or a link to a web page.
  , aPNSCIOPushImage :: Maybe Text -- ^ The URL of an HTTPS image that you want to use for your message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON APNSCIOPush where
  parseJSON = genericParseJSON optionsAPNSCIOPush
instance ToJSON APNSCIOPush where
  toJSON = genericToJSON optionsAPNSCIOPush

optionsAPNSCIOPush :: Options
optionsAPNSCIOPush =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("aPNSCIOPushLink", "link")
      , ("aPNSCIOPushImage", "image")
      ]


-- | 
data ActionObject = ActionObject
  { actionObjectId :: Maybe Int -- ^ The identifier for an action.
  , actionObjectCampaignUnderscoreid :: Maybe Int -- ^ The identifier for a campaign.
  , actionObjectParentUnderscoreactionUnderscoreid :: Maybe Int -- ^ The ID of the parent action, if the action occurred within a campaign and has a parent (like a randomized split, etc).
  , actionObjectDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , actionObjectName :: Maybe Text -- ^ The name of the action, if it exists.
  , actionObjectLayout :: Maybe Text -- ^ The layout used for the action, if it exists.
  , actionObjectCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , actionObjectUpdated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , actionObjectBody :: Maybe Text -- ^ The payload for your webhook.
  , actionObjectBodyUnderscoreamp :: Maybe Text -- ^ If your message is an email, this is the AMP-enabled body of your message. If your recipient's email client doesn't support AMP, the `body` represents your fallback message.
  , actionObjectLanguage :: Maybe Text -- ^ The language variant for your message. If you don't use our [localization feature](/localization), or this is the default message, this value is an empty string.
  , actionObjectType :: Maybe Text -- ^ The type of action.
  , actionObjectSendingUnderscorestate :: Maybe Text -- ^ Determines the sending behavior for the action. `automatic` sends the action automatically when triggered; `draft` queues drafts when the action is triggered; or `off` to disable the action.
  , actionObjectFrom :: Maybe Text -- ^ The address that the message is from, relevant if the action `type` is `email`.
  , actionObjectFromUnderscoreid :: Maybe Int -- ^ The identifier of the `from` address, commonly known as the \"sender\". You can [list your sender identities](#operation/listSenders) to match the ID to a specific address.
  , actionObjectReplyUnderscoreto :: Maybe Text -- ^ The address that receives replies for the message, if applicable.
  , actionObjectReplyUnderscoretoUnderscoreid :: Maybe Int -- ^ The identifier for the `reply_to` address, if applicable. You can [list your sender identities](#operation/listSenders) to match the ID to a specific address.
  , actionObjectPreprocessor :: Maybe Text -- ^ By default, we process CSS before emails leave Customer.io using Premailer. If your message included CSS and pre-processing is not disabled, this key indicates the pre-processor.
  , actionObjectRecipient :: Maybe Text -- ^ The recipient value. In general, your recipient is an attribute that you reference using liquid, like `{{customer.phone}}`, instead of a hard-coded value. If you set this field to a liquid statement like `{{customer.phone}}`, the field returns blank in `GET` requests because we populate the recipient from your liquid statement at send time.
  , actionObjectSubject :: Maybe Text -- ^ The subject line for an `email` action.
  , actionObjectBcc :: Maybe Text -- ^ The blind-copy address(es) for this action.
  , actionObjectFakeUnderscorebcc :: Maybe Bool -- ^ If true, rather than sending true copies to BCC addresses, Customer.io sends a copy of the message with the subject line containing the recipient address(es). 
  , actionObjectPreheaderUnderscoretext :: Maybe Text -- ^ Also known as \"preview text\", this specifies the small block of text shown in an end-user's email inbox, next to, or underneath, the subject line.
  , actionObjectHeaders :: Maybe (Map.Map String Text) -- ^ An object containing headers, where the key is the header name and the value is the header value. Header names and values must be strings and cannot contain any non-ASCII characters or empty spaces. Some headers are reserved and cannot be overwritten.
  , actionObjectImageUnderscoreurl :: Maybe Text -- ^ The URL of the image in your SMS (MMS) message.
  , actionObjectUrl :: Maybe Text -- ^ The URL to send a webhook to, applies to `webhook` type actions.
  , actionObjectMethod :: Maybe Text -- ^ The HTTP method for your webhook.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ActionObject where
  parseJSON = genericParseJSON optionsActionObject
instance ToJSON ActionObject where
  toJSON = genericToJSON optionsActionObject

optionsActionObject :: Options
optionsActionObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("actionObjectId", "id")
      , ("actionObjectCampaignUnderscoreid", "campaign_id")
      , ("actionObjectParentUnderscoreactionUnderscoreid", "parent_action_id")
      , ("actionObjectDeduplicateUnderscoreid", "deduplicate_id")
      , ("actionObjectName", "name")
      , ("actionObjectLayout", "layout")
      , ("actionObjectCreated", "created")
      , ("actionObjectUpdated", "updated")
      , ("actionObjectBody", "body")
      , ("actionObjectBodyUnderscoreamp", "body_amp")
      , ("actionObjectLanguage", "language")
      , ("actionObjectType", "type")
      , ("actionObjectSendingUnderscorestate", "sending_state")
      , ("actionObjectFrom", "from")
      , ("actionObjectFromUnderscoreid", "from_id")
      , ("actionObjectReplyUnderscoreto", "reply_to")
      , ("actionObjectReplyUnderscoretoUnderscoreid", "reply_to_id")
      , ("actionObjectPreprocessor", "preprocessor")
      , ("actionObjectRecipient", "recipient")
      , ("actionObjectSubject", "subject")
      , ("actionObjectBcc", "bcc")
      , ("actionObjectFakeUnderscorebcc", "fake_bcc")
      , ("actionObjectPreheaderUnderscoretext", "preheader_text")
      , ("actionObjectHeaders", "headers")
      , ("actionObjectImageUnderscoreurl", "image_url")
      , ("actionObjectUrl", "url")
      , ("actionObjectMethod", "method")
      ]


-- | An action is either a link or a way to dismiss your message. The &#x60;behaviour&#x60; property determines how your app treats the action.
data ActionWidget = ActionWidget
  { actionWidgetType :: Text -- ^ Defines the widget type.
  , actionWidgetAction :: Text -- ^ The link or place you want to send a person. This is either a deep link in your app, a web address, a `mailto` link, or a way to close the message (`gist://close`).
  , actionWidgetBehaviour :: Maybe Text -- ^ * `push`: pushes a new route into the navigation stack. * `system`: offloads the action onto the operating system. Actions like `mailto:support@bourbon.sh` will open the default email client. * `back`: pops the navigation stack one step back. * `retain`: retain replaces the current view with a new route. 
  , actionWidgetComponent :: Value -- ^ The component a person taps to perform the action defined in this widget.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ActionWidget where
  parseJSON = genericParseJSON optionsActionWidget
instance ToJSON ActionWidget where
  toJSON = genericToJSON optionsActionWidget

optionsActionWidget :: Options
optionsActionWidget =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("actionWidgetType", "type")
      , ("actionWidgetAction", "action")
      , ("actionWidgetBehaviour", "behaviour")
      , ("actionWidgetComponent", "component")
      ]


-- | 
data ActivityObject = ActivityObject
  { activityObjectCustomerUnderscoreid :: Maybe Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI. If your workspace supports multiple identifiers (email and ID), this value can be null.
  , activityObjectCustomerUnderscoreidentifiers :: Maybe ActivityObjectCustomerIdentifiers -- ^ 
  , activityObjectData :: Maybe ActivityObjectData -- ^ 
  , activityObjectDeliveryUnderscoreid :: Maybe Text -- ^ The message ID.
  , activityObjectDeliveryUnderscoretype :: Maybe Text -- ^ The recipient device, if applicable.
  , activityObjectId :: Maybe Text -- ^ The identifier for the action.
  , activityObjectTimestamp :: Maybe Int -- ^ The date and time when the action occurred.
  , activityObjectType :: Maybe Text -- ^ The type of activity. Types with `_o:<object_type_id>` are for objects and types with `_r:<object_type_id>` are for relationships.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ActivityObject where
  parseJSON = genericParseJSON optionsActivityObject
instance ToJSON ActivityObject where
  toJSON = genericToJSON optionsActivityObject

optionsActivityObject :: Options
optionsActivityObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("activityObjectCustomerUnderscoreid", "customer_id")
      , ("activityObjectCustomerUnderscoreidentifiers", "customer_identifiers")
      , ("activityObjectData", "data")
      , ("activityObjectDeliveryUnderscoreid", "delivery_id")
      , ("activityObjectDeliveryUnderscoretype", "delivery_type")
      , ("activityObjectId", "id")
      , ("activityObjectTimestamp", "timestamp")
      , ("activityObjectType", "type")
      ]


-- | Contains identifiers for the person represented in a response—&#x60;id&#x60;, &#x60;cio_id&#x60;, and &#x60;email&#x60; (if your workspace uses the *Email or ID* setting). If the person&#39;s &#x60;id&#x60; or &#x60;email&#x60; is not set, the value will be null.  We recommend that you use this object rather than the less descriptive &#x60;customer_id&#x60;. 
data ActivityObjectCustomerIdentifiers = ActivityObjectCustomerIdentifiers
  { activityObjectCustomerIdentifiersEmail :: Text -- ^ A person's email address, if set.
  , activityObjectCustomerIdentifiersId :: Text -- ^ A person's unique ID, if set. This is the same as the `customer_id` if present.
  , activityObjectCustomerIdentifiersCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ActivityObjectCustomerIdentifiers where
  parseJSON = genericParseJSON optionsActivityObjectCustomerIdentifiers
instance ToJSON ActivityObjectCustomerIdentifiers where
  toJSON = genericToJSON optionsActivityObjectCustomerIdentifiers

optionsActivityObjectCustomerIdentifiers :: Options
optionsActivityObjectCustomerIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("activityObjectCustomerIdentifiersEmail", "email")
      , ("activityObjectCustomerIdentifiersId", "id")
      , ("activityObjectCustomerIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | 
data ActivityObjectData = ActivityObjectData
  { activityObjectDataDelivered :: Maybe Int -- ^ The date-time when the message was delivered, if applicable.
  , activityObjectDataDeliveryUnderscoreid :: Maybe Text -- ^ The message ID.
  , activityObjectDataOpened :: Maybe Bool -- ^ Indicates whether or not a customer opened a message, if the message was delivered.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ActivityObjectData where
  parseJSON = genericParseJSON optionsActivityObjectData
instance ToJSON ActivityObjectData where
  toJSON = genericToJSON optionsActivityObjectData

optionsActivityObjectData :: Options
optionsActivityObjectData =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("activityObjectDataDelivered", "delivered")
      , ("activityObjectDataDeliveryUnderscoreid", "delivery_id")
      , ("activityObjectDataOpened", "opened")
      ]


-- | The type of activity. Types with &#x60;_o:&lt;object_type_id&gt;&#x60; are for objects and types with &#x60;_r:&lt;object_type_id&gt;&#x60; are for relationships.
data ActivityTypes = ActivityTypes
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ActivityTypes where
  parseJSON = genericParseJSON optionsActivityTypes
instance ToJSON ActivityTypes where
  toJSON = genericToJSON optionsActivityTypes

optionsActivityTypes :: Options
optionsActivityTypes =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | Assign devices to a person.
data AddDevice = AddDevice
  { addDeviceType :: Text -- ^ The operation modifies a person in Customer.io
  , addDeviceIdentifiers :: IdentifyAllOfIdentifiers -- ^ 
  , addDeviceAction :: Text -- ^ Add a mobile device to a person's profile.
  , addDeviceDevice :: AddDeviceAllOfDevice -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AddDevice where
  parseJSON = genericParseJSON optionsAddDevice
instance ToJSON AddDevice where
  toJSON = genericToJSON optionsAddDevice

optionsAddDevice :: Options
optionsAddDevice =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("addDeviceType", "type")
      , ("addDeviceIdentifiers", "identifiers")
      , ("addDeviceAction", "action")
      , ("addDeviceDevice", "device")
      ]


-- | 
data AddDevice400Response = AddDevice400Response
  { addDevice400ResponseMeta :: Maybe AddDevice400ResponseMeta -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AddDevice400Response where
  parseJSON = genericParseJSON optionsAddDevice400Response
instance ToJSON AddDevice400Response where
  toJSON = genericToJSON optionsAddDevice400Response

optionsAddDevice400Response :: Options
optionsAddDevice400Response =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("addDevice400ResponseMeta", "meta")
      ]


-- | 
data AddDevice400ResponseMeta = AddDevice400ResponseMeta
  { addDevice400ResponseMetaErrors :: Maybe [Text] -- ^ An array of errors.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AddDevice400ResponseMeta where
  parseJSON = genericParseJSON optionsAddDevice400ResponseMeta
instance ToJSON AddDevice400ResponseMeta where
  toJSON = genericToJSON optionsAddDevice400ResponseMeta

optionsAddDevice400ResponseMeta :: Options
optionsAddDevice400ResponseMeta =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("addDevice400ResponseMetaErrors", "errors")
      ]


-- | The properties representing an individual device. [Our SDK&#39;s](/sdk/) gather all the properties defined below automatically, unless you disable the &#x60;autoTrackDeviceAttributes&#x60; setting. You can reference the properties outside the &#x60;attributes&#x60; object in segments.
data AddDeviceAllOfDevice = AddDeviceAllOfDevice
  { addDeviceAllOfDeviceToken :: Text -- ^ The device token.
  , addDeviceAllOfDeviceLastUnderscoreused :: Maybe Int -- ^ The `timestamp` when you last identified this device. If you don't pass a timestamp when you add or update a device, we use the time of the request itself. Our SDKs identify a device when a person launches their app.
  , addDeviceAllOfDevicePlatform :: Text -- ^ The device/messaging platform.
  , addDeviceAllOfDeviceAttributes :: Maybe AddDeviceRequestDeviceAllOfAttributes -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AddDeviceAllOfDevice where
  parseJSON = genericParseJSON optionsAddDeviceAllOfDevice
instance ToJSON AddDeviceAllOfDevice where
  toJSON = genericToJSON optionsAddDeviceAllOfDevice

optionsAddDeviceAllOfDevice :: Options
optionsAddDeviceAllOfDevice =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("addDeviceAllOfDeviceToken", "token")
      , ("addDeviceAllOfDeviceLastUnderscoreused", "last_used")
      , ("addDeviceAllOfDevicePlatform", "platform")
      , ("addDeviceAllOfDeviceAttributes", "attributes")
      ]


-- | Define the device you want to add to the customer profile.
data AddDeviceRequest = AddDeviceRequest
  { addDeviceRequestDevice :: AddDeviceRequestDevice -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AddDeviceRequest where
  parseJSON = genericParseJSON optionsAddDeviceRequest
instance ToJSON AddDeviceRequest where
  toJSON = genericToJSON optionsAddDeviceRequest

optionsAddDeviceRequest :: Options
optionsAddDeviceRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("addDeviceRequestDevice", "device")
      ]


-- | The properties representing an individual device. [Our SDK&#39;s](/sdk/) gather all the properties defined below automatically, unless you disable the &#x60;autoTrackDeviceAttributes&#x60; setting. You can reference the properties outside the &#x60;attributes&#x60; object in segments or in Liquid.
data AddDeviceRequestDevice = AddDeviceRequestDevice
  { addDeviceRequestDeviceId :: Text -- ^ The device token.
  , addDeviceRequestDeviceLastUnderscoreused :: Maybe Int -- ^ The `timestamp` when you last identified this device. If you don't pass a timestamp when you add or update a device, we use the time of the request itself. Our SDKs identify a device when a person launches their app.
  , addDeviceRequestDevicePlatform :: Text -- ^ The device/messaging platform.
  , addDeviceRequestDeviceAttributes :: Maybe AddDeviceRequestDeviceAllOfAttributes -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AddDeviceRequestDevice where
  parseJSON = genericParseJSON optionsAddDeviceRequestDevice
instance ToJSON AddDeviceRequestDevice where
  toJSON = genericToJSON optionsAddDeviceRequestDevice

optionsAddDeviceRequestDevice :: Options
optionsAddDeviceRequestDevice =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("addDeviceRequestDeviceId", "id")
      , ("addDeviceRequestDeviceLastUnderscoreused", "last_used")
      , ("addDeviceRequestDevicePlatform", "platform")
      , ("addDeviceRequestDeviceAttributes", "attributes")
      ]


-- | Attributes that you can reference to segment your audience—like a person&#39;s attributes, but specific to a device. These can be either the attributes defined below or custom key-value attributes.
newtype AddDeviceRequestDeviceAllOfAttributes = AddDeviceRequestDeviceAllOfAttributes { unAddDeviceRequestDeviceAllOfAttributes :: (Map.Map Text Text) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | Associate multiple objects with a person.
data AddRelationships = AddRelationships
  { addRelationshipsType :: Text -- ^ The operation modifies a person in Customer.io
  , addRelationshipsIdentifiers :: IdentifyAllOfIdentifiers -- ^ 
  , addRelationshipsAction :: Text -- ^ This operation associates a person with one or more objects.
  , addRelationshipsCioUnderscorerelationships :: [IdentifyRequestCioRelationshipsRelationshipsInner] -- ^ Each object in the array represents a relationship you want to add to, or remove from, a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AddRelationships where
  parseJSON = genericParseJSON optionsAddRelationships
instance ToJSON AddRelationships where
  toJSON = genericToJSON optionsAddRelationships

optionsAddRelationships :: Options
optionsAddRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("addRelationshipsType", "type")
      , ("addRelationshipsIdentifiers", "identifiers")
      , ("addRelationshipsAction", "action")
      , ("addRelationshipsCioUnderscorerelationships", "cio_relationships")
      ]


-- | Add relationships between an object and one or more people.
data AddRelationships1 = AddRelationships1
  { addRelationships1Identifiers :: Maybe IdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers -- ^ 
  , addRelationships1Type :: Text -- ^ The operation modifies a single object—non person data.
  , addRelationships1Action :: Text -- ^ This operation associates an object with one or more people.
  , addRelationships1CioUnderscorerelationships :: [Identify1AllOfCioRelationshipsInner] -- ^ The people you want to associate with an object. Each object in the array represents a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AddRelationships1 where
  parseJSON = genericParseJSON optionsAddRelationships1
instance ToJSON AddRelationships1 where
  toJSON = genericToJSON optionsAddRelationships1

optionsAddRelationships1 :: Options
optionsAddRelationships1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("addRelationships1Identifiers", "identifiers")
      , ("addRelationships1Type", "type")
      , ("addRelationships1Action", "action")
      , ("addRelationships1CioUnderscorerelationships", "cio_relationships")
      ]


-- | The IDs of people you want to add to the segment.
data AddToSegmentRequest = AddToSegmentRequest
  { addToSegmentRequestIds :: [Text] -- ^ The customer IDs you want to add to the segment. The type of value you pass in the array corresponds to the `id_type` query parameter—`id`, `email`, or `cio_id`. Entries in the array that don't match the `id_type` are ignored.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AddToSegmentRequest where
  parseJSON = genericParseJSON optionsAddToSegmentRequest
instance ToJSON AddToSegmentRequest where
  toJSON = genericToJSON optionsAddToSegmentRequest

optionsAddToSegmentRequest :: Options
optionsAddToSegmentRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("addToSegmentRequestIds", "ids")
      ]


-- | 
data AllAndroidProps = AllAndroidProps
  { allAndroidPropsTitle :: Maybe Text -- ^ The title of your push notification.
  , allAndroidPropsBody :: Maybe Text -- ^ The body of your push notification.
  , allAndroidPropsIcon :: Maybe Text -- ^ Sets the notification icon to `myicon` for drawable resource `myicon`. If you don't send this key, FCM displays the launcher icon from your app manifest.
  , allAndroidPropsSound :: Maybe Text -- ^ The sound that plays when the device receives the notification. Supports `\"default\"` or the filename of a sound resource bundled in your app. Sound files must reside in `/res/raw/`.
  , allAndroidPropsTag :: Maybe Text -- ^ Identifier to replace existing notifications in the notification drawer. If empty, each request creates a new notification.  If you specify a tag, and a notification with the same tag is already being shown, the new notification replaces the existing one in the notification drawer.  
  , allAndroidPropsColor :: Maybe Text -- ^ The notification's icon color in `#rrggbb` format.
  , allAndroidPropsClickUnderscoreaction :: Maybe Text -- ^ The action that occurs when a user taps on the notification. Launches an activity with a matching intent filter when a person taps the notification.
  , allAndroidPropsBodyUnderscorelocUnderscorekey :: Maybe Text -- ^ The key to the body string in the app's string resources that you want to use to localize the body text to the user's current localization. See [String Resources](https://developer.android.com/guide/topics/resources/string-resource/) for more information.
  , allAndroidPropsBodyUnderscorelocUnderscorearg :: Maybe Text -- ^ Variable string values used in place of the format specifiers in `body_loc_key` to localize the body text to the user's current localization. See Formatting and Styling for more information.
  , allAndroidPropsTitleUnderscorelocUnderscorekey :: Maybe Text -- ^ The key to the title string in the app's string resources that you want to use to localize the title text to the user's current localization. See [String Resources](https://developer.android.com/guide/topics/resources/string-resource/) for more information.
  , allAndroidPropsTitleUnderscorelocUnderscorearg :: Maybe Text -- ^ Variable string values used in place of the format specifiers in `title_loc_key` to localize the title text to the user's current localization. See Formatting and Styling for more information.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AllAndroidProps where
  parseJSON = genericParseJSON optionsAllAndroidProps
instance ToJSON AllAndroidProps where
  toJSON = genericToJSON optionsAllAndroidProps

optionsAllAndroidProps :: Options
optionsAllAndroidProps =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("allAndroidPropsTitle", "title")
      , ("allAndroidPropsBody", "body")
      , ("allAndroidPropsIcon", "icon")
      , ("allAndroidPropsSound", "sound")
      , ("allAndroidPropsTag", "tag")
      , ("allAndroidPropsColor", "color")
      , ("allAndroidPropsClickUnderscoreaction", "click_action")
      , ("allAndroidPropsBodyUnderscorelocUnderscorekey", "body_loc_key")
      , ("allAndroidPropsBodyUnderscorelocUnderscorearg", "body_loc_arg")
      , ("allAndroidPropsTitleUnderscorelocUnderscorekey", "title_loc_key")
      , ("allAndroidPropsTitleUnderscorelocUnderscorearg", "title_loc_arg")
      ]


-- | 
data And = And
  { andAnd :: Maybe [ComplexAudienceFilterAndInner] -- ^ Match *all* conditions to return results.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON And where
  parseJSON = genericParseJSON optionsAnd
instance ToJSON And where
  toJSON = genericToJSON optionsAnd

optionsAnd :: Options
optionsAnd =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("andAnd", "and")
      ]


-- | 
data And1 = And1
  { and1And :: Maybe [PeopleFilter] -- ^ Match *all* conditions to return results.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON And1 where
  parseJSON = genericParseJSON optionsAnd1
instance ToJSON And1 where
  toJSON = genericToJSON optionsAnd1

optionsAnd1 :: Options
optionsAnd1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("and1And", "and")
      ]


-- | 
data And2 = And2
  { and2And :: Maybe [AndAudienceFilterAndInner] -- ^ Match *all* conditions to return results.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON And2 where
  parseJSON = genericParseJSON optionsAnd2
instance ToJSON And2 where
  toJSON = genericToJSON optionsAnd2

optionsAnd2 :: Options
optionsAnd2 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("and2And", "and")
      ]


-- | 
data And3 = And3
  { and3And :: Maybe [ObjectFilterAndAndInner] -- ^ Match *all* conditions to return results.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON And3 where
  parseJSON = genericParseJSON optionsAnd3
instance ToJSON And3 where
  toJSON = genericToJSON optionsAnd3

optionsAnd3 :: Options
optionsAnd3 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("and3And", "and")
      ]


-- | 
data AndAudienceFilter = AndAudienceFilter
  { andAudienceFilterAnd :: Maybe [AndAudienceFilterAndInner] -- ^ Match *all* conditions to return results.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AndAudienceFilter where
  parseJSON = genericParseJSON optionsAndAudienceFilter
instance ToJSON AndAudienceFilter where
  toJSON = genericToJSON optionsAndAudienceFilter

optionsAndAudienceFilter :: Options
optionsAndAudienceFilter =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("andAudienceFilterAnd", "and")
      ]


-- | 
data AndAudienceFilterAndInner = AndAudienceFilterAndInner
  { andAudienceFilterAndInnerOr :: Maybe [ComplexAudienceFilterAndInner] -- ^ Returns results matching *any* conditions.
  , andAudienceFilterAndInnerNot :: Maybe ComplexAudienceFilterNot -- ^ 
  , andAudienceFilterAndInnerSegment :: Maybe Segment -- ^ 
  , andAudienceFilterAndInnerAttribute :: Maybe Attribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AndAudienceFilterAndInner where
  parseJSON = genericParseJSON optionsAndAudienceFilterAndInner
instance ToJSON AndAudienceFilterAndInner where
  toJSON = genericToJSON optionsAndAudienceFilterAndInner

optionsAndAudienceFilterAndInner :: Options
optionsAndAudienceFilterAndInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("andAudienceFilterAndInnerOr", "or")
      , ("andAudienceFilterAndInnerNot", "not")
      , ("andAudienceFilterAndInnerSegment", "segment")
      , ("andAudienceFilterAndInnerAttribute", "attribute")
      ]


-- | An event attributed to an unknown person. If you provide an &#x60;anonymous_id&#x60; with the event, you can associate the event with a person later (using the anonymous ID).
data AnonymousEventsRequest = AnonymousEventsRequest
  { anonymousEventsRequestAnonymousUnderscoreid :: Maybe Text -- ^ An identifier for an anonymous event, like a cookie. If set as an attribute on a person, any events bearing the same anonymous value are associated with this person. This value must be unique and is not reusable.
  , anonymousEventsRequestName :: Text -- ^ The name of the event. In general, this should be the name of the screen or deep link path that a person viewed, making it easy to segment your audience or trigger campaigns using this event. Make sure you trim leading and trailing spaces from this field.
  , anonymousEventsRequestId :: Maybe Text -- ^ An identifier used to deduplicate events. This value must be a [ULID](https://github.com/ulid/spec). If an event has the same value as an event we previously received, we won't show or process the duplicate. Note - our Python and Ruby libraries do not pass this id.
  , anonymousEventsRequestType :: Text -- ^ Sets the event type. If your event isn't a `page` or `screen` type event, we automatically set this property to `event`.
  , anonymousEventsRequestTimestamp :: Maybe Int -- ^ The unix timestamp when the event took place. If you don't provide this value, we use the date-time when we receive the event. 
  , anonymousEventsRequestData :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on your customer (referenced by `customer_id`).
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AnonymousEventsRequest where
  parseJSON = genericParseJSON optionsAnonymousEventsRequest
instance ToJSON AnonymousEventsRequest where
  toJSON = genericToJSON optionsAnonymousEventsRequest

optionsAnonymousEventsRequest :: Options
optionsAnonymousEventsRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("anonymousEventsRequestAnonymousUnderscoreid", "anonymous_id")
      , ("anonymousEventsRequestName", "name")
      , ("anonymousEventsRequestId", "id")
      , ("anonymousEventsRequestType", "type")
      , ("anonymousEventsRequestTimestamp", "timestamp")
      , ("anonymousEventsRequestData", "data")
      ]


-- | 
data ApnsWithSdk = ApnsWithSdk
  { apnsWithSdkCIO :: Maybe APNSCIO -- ^ 
  , apnsWithSdkAps :: Maybe FCMMessageApnsPayloadAps -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ApnsWithSdk where
  parseJSON = genericParseJSON optionsApnsWithSdk
instance ToJSON ApnsWithSdk where
  toJSON = genericToJSON optionsApnsWithSdk

optionsApnsWithSdk :: Options
optionsApnsWithSdk =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("apnsWithSdkCIO", "CIO")
      , ("apnsWithSdkAps", "aps")
      ]


-- | 
data ApnsWithoutSdk = ApnsWithoutSdk
  { apnsWithoutSdkAps :: Maybe FCMMessageApnsPayloadAps -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ApnsWithoutSdk where
  parseJSON = genericParseJSON optionsApnsWithoutSdk
instance ToJSON ApnsWithoutSdk where
  toJSON = genericToJSON optionsApnsWithoutSdk

optionsApnsWithoutSdk :: Options
optionsApnsWithoutSdk =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("apnsWithoutSdkAps", "aps")
      ]


-- | 
data ApsBasicPush = ApsBasicPush
  { apsBasicPushAps :: ApsBasicPushAps -- ^ 
  , apsBasicPushCIO :: Maybe APNSCIO -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ApsBasicPush where
  parseJSON = genericParseJSON optionsApsBasicPush
instance ToJSON ApsBasicPush where
  toJSON = genericToJSON optionsApsBasicPush

optionsApsBasicPush :: Options
optionsApsBasicPush =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("apsBasicPushAps", "aps")
      , ("apsBasicPushCIO", "CIO")
      ]


-- | A push payload intended for an iOS device.
data ApsBasicPushAps = ApsBasicPushAps
  { apsBasicPushApsAlert :: ApsBasicPushApsAlert -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ApsBasicPushAps where
  parseJSON = genericParseJSON optionsApsBasicPushAps
instance ToJSON ApsBasicPushAps where
  toJSON = genericToJSON optionsApsBasicPushAps

optionsApsBasicPushAps :: Options
optionsApsBasicPushAps =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("apsBasicPushApsAlert", "alert")
      ]


-- | An object containing the push title and body
data ApsBasicPushApsAlert = ApsBasicPushApsAlert
  { apsBasicPushApsAlertTitle :: Maybe Text -- ^ The title of your push notification.
  , apsBasicPushApsAlertBody :: Text -- ^ The body of your push notification.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ApsBasicPushApsAlert where
  parseJSON = genericParseJSON optionsApsBasicPushApsAlert
instance ToJSON ApsBasicPushApsAlert where
  toJSON = genericToJSON optionsApsBasicPushApsAlert

optionsApsBasicPushApsAlert :: Options
optionsApsBasicPushApsAlert =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("apsBasicPushApsAlertTitle", "title")
      , ("apsBasicPushApsAlertBody", "body")
      ]


-- | An archived message, including a complete message body. 
data ArchivedMessageObject = ArchivedMessageObject
  { archivedMessageObjectId :: Maybe Text -- ^ The identifier for a delivery—the instance of a message intended for an individual recipient.
  , archivedMessageObjectBody :: Maybe Text -- ^ The body of the variant. You cannot modify the body if you created it with our drag-and-drop editor.
  , archivedMessageObjectFrom :: Maybe Text -- ^ The address that the message is from, relevant if the action `type` is `email`.
  , archivedMessageObjectReplyUnderscoreto :: Maybe Text -- ^ The address that receives replies for the message, if applicable.
  , archivedMessageObjectRecipient :: Maybe Text -- ^ The recipient address for an action.
  , archivedMessageObjectSubject :: Maybe Text -- ^ The subject line for an `email` action.
  , archivedMessageObjectBcc :: Maybe Text -- ^ The blind-copy address(es) for this action.
  , archivedMessageObjectFakeUnderscorebcc :: Maybe Bool -- ^ If true, rather than sending true copies to BCC addresses, Customer.io sends a copy of the message with the subject line containing the recipient address(es). 
  , archivedMessageObjectPreheaderUnderscoretext :: Maybe Text -- ^ Also known as \"preview text\", this specifies the small block of text shown in an end-user's email inbox, next to, or underneath, the subject line.
  , archivedMessageObjectUrl :: Maybe Text -- ^ The URL of a webhook or action.
  , archivedMessageObjectRequestUnderscoremethod :: Maybe Text -- ^ The method used in conjunction with a webhook `url`.
  , archivedMessageObjectHeaders :: Maybe (Map.Map String Text) -- ^ An object containing headers, where the key is the header name and the value is the header value. Header names and values must be strings and cannot contain any non-ASCII characters or empty spaces. Some headers are reserved and cannot be overwritten.
  , archivedMessageObjectForgotten :: Maybe Bool -- ^ If true, Customer.io does not retain the message content.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ArchivedMessageObject where
  parseJSON = genericParseJSON optionsArchivedMessageObject
instance ToJSON ArchivedMessageObject where
  toJSON = genericToJSON optionsArchivedMessageObject

optionsArchivedMessageObject :: Options
optionsArchivedMessageObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("archivedMessageObjectId", "id")
      , ("archivedMessageObjectBody", "body")
      , ("archivedMessageObjectFrom", "from")
      , ("archivedMessageObjectReplyUnderscoreto", "reply_to")
      , ("archivedMessageObjectRecipient", "recipient")
      , ("archivedMessageObjectSubject", "subject")
      , ("archivedMessageObjectBcc", "bcc")
      , ("archivedMessageObjectFakeUnderscorebcc", "fake_bcc")
      , ("archivedMessageObjectPreheaderUnderscoretext", "preheader_text")
      , ("archivedMessageObjectUrl", "url")
      , ("archivedMessageObjectRequestUnderscoremethod", "request_method")
      , ("archivedMessageObjectHeaders", "headers")
      , ("archivedMessageObjectForgotten", "forgotten")
      ]


-- | A dictionary of attachments where the filename is the key and the value is the base64-encoded contents. The filename must include the extension (i.e. &#x60;name.csv&#x60;). The total size of all attachments must be less than 2 MB.
data Attachments = Attachments
  { attachmentsLessThanfileDashnameGreaterThan :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Attachments where
  parseJSON = genericParseJSON optionsAttachments
instance ToJSON Attachments where
  toJSON = genericToJSON optionsAttachments

optionsAttachments :: Options
optionsAttachments =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("attachmentsLessThanfileDashnameGreaterThan", "<file-name>")
      ]


-- | Filter your audience by attribute.
data Attribute = Attribute
  { attributeField :: Text -- ^ The name of the attribute you want to filter against.
  , attributeOperator :: Text -- ^ Determine how to evaluate criteria against the field—`exists` returns results if a person in the audience has the attribute; `eq` returns results if the audience has the attribute and the attribute has the `value` you specify.
  , attributeValue :: Maybe Text -- ^ The value you want to match for this attribute. You must include a value if you use the `eq` operator.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Attribute where
  parseJSON = genericParseJSON optionsAttribute
instance ToJSON Attribute where
  toJSON = genericToJSON optionsAttribute

optionsAttribute :: Options
optionsAttribute =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("attributeField", "field")
      , ("attributeOperator", "operator")
      , ("attributeValue", "value")
      ]


-- | 
data Attribute1 = Attribute1
  { attribute1Attribute :: Maybe Attribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Attribute1 where
  parseJSON = genericParseJSON optionsAttribute1
instance ToJSON Attribute1 where
  toJSON = genericToJSON optionsAttribute1

optionsAttribute1 :: Options
optionsAttribute1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("attribute1Attribute", "attribute")
      ]


-- | Filter your audience by attribute.
data AttributeAudienceFilter = AttributeAudienceFilter
  { attributeAudienceFilterField :: Text -- ^ The name of the attribute you want to filter against.
  , attributeAudienceFilterOperator :: Text -- ^ Determine how to evaluate criteria against the field—`exists` returns results if a person in the audience has the attribute; `eq` returns results if the audience has the attribute and the attribute has the `value` you specify.
  , attributeAudienceFilterValue :: Maybe Text -- ^ The value you want to match for this attribute. You must include a value if you use the `eq` operator.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AttributeAudienceFilter where
  parseJSON = genericParseJSON optionsAttributeAudienceFilter
instance ToJSON AttributeAudienceFilter where
  toJSON = genericToJSON optionsAttributeAudienceFilter

optionsAttributeAudienceFilter :: Options
optionsAttributeAudienceFilter =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("attributeAudienceFilterField", "field")
      , ("attributeAudienceFilterOperator", "operator")
      , ("attributeAudienceFilterValue", "value")
      ]


-- | 
data AttributeChangeAction = AttributeChangeAction
  { attributeChangeActionFrom :: Maybe Text -- ^ The old attribute value. If empty, the customer probably didn't bear the attribute before this action.
  , attributeChangeActionTo :: Maybe Text -- ^ The new attribute value.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AttributeChangeAction where
  parseJSON = genericParseJSON optionsAttributeChangeAction
instance ToJSON AttributeChangeAction where
  toJSON = genericToJSON optionsAttributeChangeAction

optionsAttributeChangeAction :: Options
optionsAttributeChangeAction =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("attributeChangeActionFrom", "from")
      , ("attributeChangeActionTo", "to")
      ]


-- | 
data AttributeChangeValue = AttributeChangeValue
  { attributeChangeValueFrom :: Maybe Text -- ^ The old attribute value. If empty, the customer probably didn't bear the attribute before this action.
  , attributeChangeValueTo :: Maybe Text -- ^ The new attribute value.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AttributeChangeValue where
  parseJSON = genericParseJSON optionsAttributeChangeValue
instance ToJSON AttributeChangeValue where
  toJSON = genericToJSON optionsAttributeChangeValue

optionsAttributeChangeValue :: Options
optionsAttributeChangeValue =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("attributeChangeValueFrom", "from")
      , ("attributeChangeValueTo", "to")
      ]


-- | filter for people who have an attribute or an attribute value.
data Audience = Audience
  { audienceAttribute :: Maybe Attribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Audience where
  parseJSON = genericParseJSON optionsAudience
instance ToJSON Audience where
  toJSON = genericToJSON optionsAudience

optionsAudience :: Options
optionsAudience =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("audienceAttribute", "attribute")
      ]


-- | A filter defining the group of people that you want send the broadcast to.
data AudienceFilter = AudienceFilter
  { audienceFilterAnd :: Maybe [PeopleFilter] -- ^ Match *all* conditions to return results.
  , audienceFilterOr :: Maybe [PeopleFilter] -- ^ Match *any* condition to return results.
  , audienceFilterSegment :: Maybe Segment -- ^ 
  , audienceFilterAttribute :: Maybe Attribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AudienceFilter where
  parseJSON = genericParseJSON optionsAudienceFilter
instance ToJSON AudienceFilter where
  toJSON = genericToJSON optionsAudienceFilter

optionsAudienceFilter :: Options
optionsAudienceFilter =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("audienceFilterAnd", "and")
      , ("audienceFilterOr", "or")
      , ("audienceFilterSegment", "segment")
      , ("audienceFilterAttribute", "attribute")
      ]


-- | 
data Batch207Response = Batch207Response
  { batch207ResponseErrors :: Maybe [Batch207ResponseErrorsInner] -- ^ An array of objects, where each object represents an error. The `batch_index` field for each object is the 0-indexed position of the failing object in your request.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Batch207Response where
  parseJSON = genericParseJSON optionsBatch207Response
instance ToJSON Batch207Response where
  toJSON = genericToJSON optionsBatch207Response

optionsBatch207Response :: Options
optionsBatch207Response =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("batch207ResponseErrors", "errors")
      ]


-- | 
data Batch207ResponseErrorsInner = Batch207ResponseErrorsInner
  { batch207ResponseErrorsInnerBatchUnderscoreindex :: Maybe Int -- ^ The 0-indexed position of the failing object in your request.
  , batch207ResponseErrorsInnerReason :: Maybe Text -- ^ The reason for the error.
  , batch207ResponseErrorsInnerField :: Maybe Text -- ^ The field containing the error.
  , batch207ResponseErrorsInnerMessage :: Maybe Text -- ^ A detailed description of the error in the offending field.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Batch207ResponseErrorsInner where
  parseJSON = genericParseJSON optionsBatch207ResponseErrorsInner
instance ToJSON Batch207ResponseErrorsInner where
  toJSON = genericToJSON optionsBatch207ResponseErrorsInner

optionsBatch207ResponseErrorsInner :: Options
optionsBatch207ResponseErrorsInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("batch207ResponseErrorsInnerBatchUnderscoreindex", "batch_index")
      , ("batch207ResponseErrorsInnerReason", "reason")
      , ("batch207ResponseErrorsInnerField", "field")
      , ("batch207ResponseErrorsInnerMessage", "message")
      ]


-- | 
data BatchRequest = BatchRequest
  { batchRequestBatch :: Maybe [BatchRequestBatchInner] -- ^ A batch of requests, where each object is any individual [entity payload](##tag/v2_entity/operation/entity)—modifying a single person or object.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BatchRequest where
  parseJSON = genericParseJSON optionsBatchRequest
instance ToJSON BatchRequest where
  toJSON = genericToJSON optionsBatchRequest

optionsBatchRequest :: Options
optionsBatchRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("batchRequestBatch", "batch")
      ]


-- | 
data BatchRequestBatchInner = BatchRequestBatchInner
  { batchRequestBatchInnerType :: Text -- ^ The operation modifies a person in Customer.io
  , batchRequestBatchInnerIdentifiers :: DeliveryIdentifiers -- ^ 
  , batchRequestBatchInnerAction :: Text -- ^ An `event` action indicates a delivery event. Use the `name` to determine the specific metric that you want to attribute to this delivery.
  , batchRequestBatchInnerAttributes :: DeliveryAttributes -- ^ 
  , batchRequestBatchInnerCioUnderscorerelationships :: [Identify1AllOfCioRelationshipsInner] -- ^ The people you want to associate with an object. Each object in the array represents a person.
  , batchRequestBatchInnerId :: Maybe Text -- ^ A valid ULID used to deduplicate events. Note - our Python and Ruby libraries do not pass this id.
  , batchRequestBatchInnerName :: Text -- ^ The name of the metric you want to attribute to this \"delivery\".
  , batchRequestBatchInnerTimestamp :: Maybe Int -- ^ The Unix timestamp when the event happened.
  , batchRequestBatchInnerDevice :: DeleteDeviceAllOfDevice -- ^ 
  , batchRequestBatchInnerPrimary :: PersonOneOf3Primary -- ^ 
  , batchRequestBatchInnerSecondary :: PersonOneOf3Secondary -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BatchRequestBatchInner where
  parseJSON = genericParseJSON optionsBatchRequestBatchInner
instance ToJSON BatchRequestBatchInner where
  toJSON = genericToJSON optionsBatchRequestBatchInner

optionsBatchRequestBatchInner :: Options
optionsBatchRequestBatchInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("batchRequestBatchInnerType", "type")
      , ("batchRequestBatchInnerIdentifiers", "identifiers")
      , ("batchRequestBatchInnerAction", "action")
      , ("batchRequestBatchInnerAttributes", "attributes")
      , ("batchRequestBatchInnerCioUnderscorerelationships", "cio_relationships")
      , ("batchRequestBatchInnerId", "id")
      , ("batchRequestBatchInnerName", "name")
      , ("batchRequestBatchInnerTimestamp", "timestamp")
      , ("batchRequestBatchInnerDevice", "device")
      , ("batchRequestBatchInnerPrimary", "primary")
      , ("batchRequestBatchInnerSecondary", "secondary")
      ]


-- | A widget that provides visual design for one or more child components.
data BlockWidget = BlockWidget
  { blockWidgetType :: Maybe Text -- ^ Defines the widget type.
  , blockWidgetSafeInsets :: Maybe Bool -- ^ Based on the env `safe-area-inset-*` CSS properties. Set to true to ensure that the block can't overflow the defined screen or the defined area of your message. Defaults to `false`.
  , blockWidgetPadding :: Maybe [Text] -- ^ Defines padding for the block, based on the values set under *Branding* > *Padding*. As with the CSS `padding` property, values in the array represent top, right, bottom, and left padding.
  , blockWidgetBackgroundColor :: Maybe Text -- ^ The background color for your block. You must set a value defined under *Branding* > *Colors*.
  , blockWidgetBorderColor :: Maybe Text -- ^ The border color for your block, if you set a border width greater than 0. You must set a value defined under *Branding* > *Colors*.
  , blockWidgetBorderWidth :: Maybe Int -- ^ The width of the border for this block in pixels.
  , blockWidgetBorderRadius :: Maybe Int -- ^ Sets the radius of corners for an item in pixels, similar to the `border-radius` CSS property.
  , blockWidgetHeight :: Maybe Int -- ^ The height of the component in pixels, if you want to constrain it. If you don't set a height or width, we'll scale your content to fit your message or container.
  , blockWidgetBackgroundImage :: Maybe Text -- ^ Set a background image for the block
  , blockWidgetFlex :: Maybe Int -- ^ The single digit syntax for the CSS `flex` property. The value you use here determines the propotional amount of space the block consumes in a parent container.
  , blockWidgetComponents :: Maybe [Value] -- ^ An array of child components that you want to make available inside this widget.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BlockWidget where
  parseJSON = genericParseJSON optionsBlockWidget
instance ToJSON BlockWidget where
  toJSON = genericToJSON optionsBlockWidget

optionsBlockWidget :: Options
optionsBlockWidget =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("blockWidgetType", "type")
      , ("blockWidgetSafeInsets", "safeInsets")
      , ("blockWidgetPadding", "padding")
      , ("blockWidgetBackgroundColor", "backgroundColor")
      , ("blockWidgetBorderColor", "borderColor")
      , ("blockWidgetBorderWidth", "borderWidth")
      , ("blockWidgetBorderRadius", "borderRadius")
      , ("blockWidgetHeight", "height")
      , ("blockWidgetBackgroundImage", "backgroundImage")
      , ("blockWidgetFlex", "flex")
      , ("blockWidgetComponents", "components")
      ]


-- | 
data BroadcastActionObject = BroadcastActionObject
  { broadcastActionObjectId :: Maybe Int -- ^ The identifier for an action.
  , broadcastActionObjectBroadcastUnderscoreid :: Maybe Int -- ^ The identifier for a broadcast.
  , broadcastActionObjectDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , broadcastActionObjectName :: Maybe Text -- ^ The name of the action, if it exists.
  , broadcastActionObjectLayout :: Maybe Text -- ^ The layout used for the action, if it exists.
  , broadcastActionObjectCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , broadcastActionObjectUpdated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , broadcastActionObjectBody :: Maybe Text -- ^ The body of the action. You cannot modify the body if you created it with our drag-and-drop editor.
  , broadcastActionObjectType :: Maybe Text -- ^ The type of action.
  , broadcastActionObjectSendingUnderscorestate :: Maybe Text -- ^ Determines the sending behavior for the action. `automatic` sends the action automatically when triggered; `draft` queues drafts when the action is triggered; or `off` to disable the action.
  , broadcastActionObjectLanguage :: Maybe Text -- ^ The language variant for your message. If you don't use our [localization feature](/localization), or this is the default message, this value is an empty string.
  , broadcastActionObjectFrom :: Maybe Text -- ^ The address that the message is from, relevant if the action `type` is `email`.
  , broadcastActionObjectFromUnderscoreid :: Maybe Int -- ^ The identifier of the `from` address, commonly known as the \"sender\". You can [list your sender identities](#operation/listSenders) to match the ID to a specific address.
  , broadcastActionObjectReplyUnderscoreto :: Maybe Text -- ^ The address that receives replies for the message, if applicable.
  , broadcastActionObjectReplyUnderscoretoUnderscoreid :: Maybe Int -- ^ The identifier for the `reply_to` address, if applicable. You can [list your sender identities](#operation/listSenders) to match the ID to a specific address.
  , broadcastActionObjectPreprocessor :: Maybe Text -- ^ By default, we process CSS before emails leave Customer.io using Premailer. If your message included CSS and pre-processing is not disabled, this key indicates the pre-processor.
  , broadcastActionObjectRecipient :: Maybe Text -- ^ The recipient value. In general, your recipient is an attribute that you reference using liquid, like `{{customer.phone}}`, instead of a hard-coded value. If you set this field to a liquid statement like `{{customer.phone}}`, the field returns blank in `GET` requests because we populate the recipient from your liquid statement at send time.
  , broadcastActionObjectSubject :: Maybe Text -- ^ The subject line for an `email` action.
  , broadcastActionObjectBcc :: Maybe Text -- ^ The blind-copy address(es) for this action.
  , broadcastActionObjectFakeUnderscorebcc :: Maybe Bool -- ^ If true, rather than sending true copies to BCC addresses, Customer.io sends a copy of the message with the subject line containing the recipient address(es). 
  , broadcastActionObjectPreheaderUnderscoretext :: Maybe Text -- ^ Also known as \"preview text\", this specifies the small block of text shown in an end-user's email inbox, next to, or underneath, the subject line.
  , broadcastActionObjectHeaders :: Maybe (Map.Map String Text) -- ^ An object containing headers, where the key is the header name and the value is the header value. Header names and values must be strings and cannot contain any non-ASCII characters or empty spaces. Some headers are reserved and cannot be overwritten.
  , broadcastActionObjectBodyUnderscoreamp :: Maybe Text -- ^ If your message is an email, this is the AMP-enabled body of your message. If your recipient's email client doesn't support AMP, the `body` represents your fallback message.
  , broadcastActionObjectImageUnderscoreurl :: Maybe Text -- ^ The URL of the image in your SMS (MMS) message.
  , broadcastActionObjectUrl :: Maybe Text -- ^ The URL to send a webhook to, applies to `webhook` type actions.
  , broadcastActionObjectMethod :: Maybe Text -- ^ The HTTP method for your webhook.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BroadcastActionObject where
  parseJSON = genericParseJSON optionsBroadcastActionObject
instance ToJSON BroadcastActionObject where
  toJSON = genericToJSON optionsBroadcastActionObject

optionsBroadcastActionObject :: Options
optionsBroadcastActionObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("broadcastActionObjectId", "id")
      , ("broadcastActionObjectBroadcastUnderscoreid", "broadcast_id")
      , ("broadcastActionObjectDeduplicateUnderscoreid", "deduplicate_id")
      , ("broadcastActionObjectName", "name")
      , ("broadcastActionObjectLayout", "layout")
      , ("broadcastActionObjectCreated", "created")
      , ("broadcastActionObjectUpdated", "updated")
      , ("broadcastActionObjectBody", "body")
      , ("broadcastActionObjectType", "type")
      , ("broadcastActionObjectSendingUnderscorestate", "sending_state")
      , ("broadcastActionObjectLanguage", "language")
      , ("broadcastActionObjectFrom", "from")
      , ("broadcastActionObjectFromUnderscoreid", "from_id")
      , ("broadcastActionObjectReplyUnderscoreto", "reply_to")
      , ("broadcastActionObjectReplyUnderscoretoUnderscoreid", "reply_to_id")
      , ("broadcastActionObjectPreprocessor", "preprocessor")
      , ("broadcastActionObjectRecipient", "recipient")
      , ("broadcastActionObjectSubject", "subject")
      , ("broadcastActionObjectBcc", "bcc")
      , ("broadcastActionObjectFakeUnderscorebcc", "fake_bcc")
      , ("broadcastActionObjectPreheaderUnderscoretext", "preheader_text")
      , ("broadcastActionObjectHeaders", "headers")
      , ("broadcastActionObjectBodyUnderscoreamp", "body_amp")
      , ("broadcastActionObjectImageUnderscoreurl", "image_url")
      , ("broadcastActionObjectUrl", "url")
      , ("broadcastActionObjectMethod", "method")
      ]


-- | 
data BroadcastObject = BroadcastObject
  { broadcastObjectId :: Maybe Int -- ^ The identifier for a broadcast.
  , broadcastObjectDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , broadcastObjectName :: Maybe Text -- ^ The name of the broadcast.
  , broadcastObjectType :: Maybe Text -- ^ The type of broadcast.
  , broadcastObjectCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , broadcastObjectUpdated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , broadcastObjectActive :: Maybe Bool -- ^ If true, the broadcast is active.
  , broadcastObjectState :: Maybe Text -- ^ The state of the broadcast.
  , broadcastObjectActions :: Maybe [BroadcastObjectActionsInner] -- ^ A list of actions used by the broadcast.
  , broadcastObjectMsgUnderscoretemplateUnderscoreids :: Maybe [MsgTemplateIdsInner] -- ^ Indicates the message template(s) used in this broadcast.
  , broadcastObjectFirstUnderscorestarted :: Maybe Int -- ^ The date and time when you activated the broadcast.
  , broadcastObjectTags :: Maybe [Text] -- ^ An array of tags you set on this broadcast.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BroadcastObject where
  parseJSON = genericParseJSON optionsBroadcastObject
instance ToJSON BroadcastObject where
  toJSON = genericToJSON optionsBroadcastObject

optionsBroadcastObject :: Options
optionsBroadcastObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("broadcastObjectId", "id")
      , ("broadcastObjectDeduplicateUnderscoreid", "deduplicate_id")
      , ("broadcastObjectName", "name")
      , ("broadcastObjectType", "type")
      , ("broadcastObjectCreated", "created")
      , ("broadcastObjectUpdated", "updated")
      , ("broadcastObjectActive", "active")
      , ("broadcastObjectState", "state")
      , ("broadcastObjectActions", "actions")
      , ("broadcastObjectMsgUnderscoretemplateUnderscoreids", "msg_template_ids")
      , ("broadcastObjectFirstUnderscorestarted", "first_started")
      , ("broadcastObjectTags", "tags")
      ]


-- | 
data BroadcastObjectActionsInner = BroadcastObjectActionsInner
  { broadcastObjectActionsInnerId :: Maybe Int -- ^ The identifier for the action.
  , broadcastObjectActionsInnerType :: Maybe Text -- ^ The type of action.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BroadcastObjectActionsInner where
  parseJSON = genericParseJSON optionsBroadcastObjectActionsInner
instance ToJSON BroadcastObjectActionsInner where
  toJSON = genericToJSON optionsBroadcastObjectActionsInner

optionsBroadcastObjectActionsInner :: Options
optionsBroadcastObjectActionsInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("broadcastObjectActionsInnerId", "id")
      , ("broadcastObjectActionsInnerType", "type")
      ]


-- | You can get triggers to know when you triggered a broadcast and whether or not it&#39;s been processed.
data BroadcastTriggerObject = BroadcastTriggerObject
  { broadcastTriggerObjectId :: Maybe Int -- ^ The identifier for a broadcast trigger.
  , broadcastTriggerObjectBroadcastUnderscoreid :: Maybe Int -- ^ The identifier for a broadcast.
  , broadcastTriggerObjectCreatedUnderscoreat :: Maybe Int -- ^ The date time when the referenced ID was created.
  , broadcastTriggerObjectProcessedUnderscoreat :: Maybe Int -- ^ The date-time when Customer.io processed the trigger.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BroadcastTriggerObject where
  parseJSON = genericParseJSON optionsBroadcastTriggerObject
instance ToJSON BroadcastTriggerObject where
  toJSON = genericToJSON optionsBroadcastTriggerObject

optionsBroadcastTriggerObject :: Options
optionsBroadcastTriggerObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("broadcastTriggerObjectId", "id")
      , ("broadcastTriggerObjectBroadcastUnderscoreid", "broadcast_id")
      , ("broadcastTriggerObjectCreatedUnderscoreat", "created_at")
      , ("broadcastTriggerObjectProcessedUnderscoreat", "processed_at")
      ]


-- | 
data CIOObjectID = CIOObjectID
  { cIOObjectIDCioUnderscoreobjectUnderscoreid :: Text -- ^ A unique value that Customer.io sets for an object when you create it. This ID is immutable.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CIOObjectID where
  parseJSON = genericParseJSON optionsCIOObjectID
instance ToJSON CIOObjectID where
  toJSON = genericToJSON optionsCIOObjectID

optionsCIOObjectID :: Options
optionsCIOObjectID =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cIOObjectIDCioUnderscoreobjectUnderscoreid", "cio_object_id")
      ]


-- | 
data CIOObjectIDUpdatesOnly = CIOObjectIDUpdatesOnly
  { cIOObjectIDUpdatesOnlyCioUnderscoreobjectUnderscoreid :: Text -- ^ A unique value that Customer.io sets for an object when you create it. This ID is immutable.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CIOObjectIDUpdatesOnly where
  parseJSON = genericParseJSON optionsCIOObjectIDUpdatesOnly
instance ToJSON CIOObjectIDUpdatesOnly where
  toJSON = genericToJSON optionsCIOObjectIDUpdatesOnly

optionsCIOObjectIDUpdatesOnly :: Options
optionsCIOObjectIDUpdatesOnly =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cIOObjectIDUpdatesOnlyCioUnderscoreobjectUnderscoreid", "cio_object_id")
      ]


-- | 
data CampaignObject = CampaignObject
  { campaignObjectId :: Maybe Int -- ^ The identifier for a campaign.
  , campaignObjectDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , campaignObjectName :: Maybe Text -- ^ The name of the campaign.
  , campaignObjectType :: Maybe Text -- ^ The type of campaign trigger. **Sunsetting on March 30, 2025**
  , campaignObjectCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , campaignObjectUpdated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , campaignObjectActive :: Maybe Bool -- ^ If true, the campaign is active and can still send messages.
  , campaignObjectState :: Maybe Text -- ^ The status of the campaign.
  , campaignObjectActions :: Maybe [SegmentActionsInner] -- ^ An array of actions contained within the campaign.
  , campaignObjectFirstUnderscorestarted :: Maybe Int -- ^ The date and time when you first started the campaign and it first became eligible to be triggered.
  , campaignObjectTags :: Maybe [Text] -- ^ An array of tags you set on this campaign.
  , campaignObjectTriggerUnderscoresegmentUnderscoreids :: Maybe [Int] -- ^ A list of segments used in the campaign trigger, returned if the campaign trigger included one or more segment conditions.
  , campaignObjectFilterUnderscoresegmentUnderscoreids :: Maybe [Int] -- ^ A list of segments used in the campaign filter, returned if the campaign audience was filtered on one or more segments.
  , campaignObjectMsgUnderscoretemplates :: Maybe [SegmentMsgTemplatesInner] -- ^ Indicates the message templates used in this campaign.
  , campaignObjectEventUnderscorename :: Maybe Text -- ^ The name of the event. How you reference the event in campaigns or segments.
  , campaignObjectFrequency :: Maybe Text -- ^ How often a person will receive this campaign based on the date specified in the campaign trigger.
  , campaignObjectDateUnderscoreattribute :: Maybe Text -- ^ The attribute on people's profiles you use to configure the date of the campaign trigger.
  , campaignObjectTimezone :: Maybe Text -- ^ The timezone you set to configure the date of the campaign trigger.
  , campaignObjectUseUnderscorecustomerUnderscoretimezone :: Maybe Bool -- ^ If you chose \"the user's timezone\" while configuring the date of the campaign trigger, this is `true`. Otherwise, you set a specific timezone so it's `false`.
  , campaignObjectStartUnderscorehour :: Maybe Int -- ^ The hour you set the campaign to trigger. Follows the 24-hour clock.
  , campaignObjectStartUnderscoreminutes :: Maybe Int -- ^ The minutes you set the campaign to trigger. Follows the 24-hour clock.
  , campaignObjectObjectUnderscoretypeUnderscoreid :: Maybe Int -- ^ The the object type ID of the trigger.
  , campaignObjectFilterUnderscoreobjectUnderscoreattributes :: Maybe Text -- ^ A list of object attributes used in the campaign filter, returned if the campaign audience was filtered on one or more object attributes.
  , campaignObjectFilterUnderscorerelationshipUnderscoreattributes :: Maybe Text -- ^ A list of relationship attributes used in the campaign filter, returned if the campaign audience was filtered on one or more relationship attributes.
  , campaignObjectAudience :: Maybe Object2Audience -- ^ 
  , campaignObjectRelationshipUnderscoreattributeUnderscoretriggers :: Maybe Value -- ^ A list of relationship attributes used to trigger the campaign.
  , campaignObjectObjectUnderscoreattributeUnderscoretriggers :: Maybe Value -- ^ A list of object attributes used to trigger the campaign.
  , campaignObjectWebhookUnderscoreid :: Maybe Int -- ^ The ID of the webhook trigger generated by Customer.io.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CampaignObject where
  parseJSON = genericParseJSON optionsCampaignObject
instance ToJSON CampaignObject where
  toJSON = genericToJSON optionsCampaignObject

optionsCampaignObject :: Options
optionsCampaignObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("campaignObjectId", "id")
      , ("campaignObjectDeduplicateUnderscoreid", "deduplicate_id")
      , ("campaignObjectName", "name")
      , ("campaignObjectType", "type")
      , ("campaignObjectCreated", "created")
      , ("campaignObjectUpdated", "updated")
      , ("campaignObjectActive", "active")
      , ("campaignObjectState", "state")
      , ("campaignObjectActions", "actions")
      , ("campaignObjectFirstUnderscorestarted", "first_started")
      , ("campaignObjectTags", "tags")
      , ("campaignObjectTriggerUnderscoresegmentUnderscoreids", "trigger_segment_ids")
      , ("campaignObjectFilterUnderscoresegmentUnderscoreids", "filter_segment_ids")
      , ("campaignObjectMsgUnderscoretemplates", "msg_templates")
      , ("campaignObjectEventUnderscorename", "event_name")
      , ("campaignObjectFrequency", "frequency")
      , ("campaignObjectDateUnderscoreattribute", "date_attribute")
      , ("campaignObjectTimezone", "timezone")
      , ("campaignObjectUseUnderscorecustomerUnderscoretimezone", "use_customer_timezone")
      , ("campaignObjectStartUnderscorehour", "start_hour")
      , ("campaignObjectStartUnderscoreminutes", "start_minutes")
      , ("campaignObjectObjectUnderscoretypeUnderscoreid", "object_type_id")
      , ("campaignObjectFilterUnderscoreobjectUnderscoreattributes", "filter_object_attributes")
      , ("campaignObjectFilterUnderscorerelationshipUnderscoreattributes", "filter_relationship_attributes")
      , ("campaignObjectAudience", "audience")
      , ("campaignObjectRelationshipUnderscoreattributeUnderscoretriggers", "relationship_attribute_triggers")
      , ("campaignObjectObjectUnderscoreattributeUnderscoretriggers", "object_attribute_triggers")
      , ("campaignObjectWebhookUnderscoreid", "webhook_id")
      ]


-- | 
data CampaignRequest = CampaignRequest
  { campaignRequestData :: Maybe (Map.Map String Value) -- ^ Contains information you want to use to populate your broadcast.
  , campaignRequestEmailUnderscoreaddUnderscoreduplicates :: Maybe Bool -- ^ an email address associated with more than one profile id is an error.
  , campaignRequestEmailUnderscoreignoreUnderscoremissing :: Maybe Bool -- ^ If false a missing email address is an error.
  , campaignRequestIdUnderscoreignoreUnderscoremissing :: Maybe Bool -- ^ If false, a missing customer ID is an error.
  , campaignRequestRecipients :: AudienceFilter -- ^ 
  , campaignRequestEmails :: [Text] -- ^ An array of email addresses you want to send the broadcast to. These addresses must already exist; your request cannot create a new person.
  , campaignRequestIds :: [Text] -- ^ An array of IDs you want to send a broadcast to. **NOTE**: If your workspace identifies people by `email`, don't use this option. Identify your audience by `emails` instead. 
  , campaignRequestPerUnderscoreuserUnderscoredata :: [UserMapsAllOfPerUserDataInner] -- ^ An array of people you want to send a broadcast to and custom data for each person. Each object in the array represents a person, with additional data you want to use to personalize their message. **When you trigger a broadcast, the people in your request must already exist in your workspace.** Requests to trigger a broadcast cannot create new people. 
  , campaignRequestDataUnderscorefileUnderscoreurl :: Text -- ^ The URL of a data file containing per-user data. Each line is a single object representing a person: either a json map of `id` and `data` or `email` and `data` keys. For example, `{\"email\":\"road@runner.net\",\"data\":{\"voucher_code\": \"coyote\"}}`.  The IDs or emails in your data file must already exist in your workspace. Your broadcast cannot add new people to your workspace and won't send messages for IDs or email addresses that don't match a person in your workspace. 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CampaignRequest where
  parseJSON = genericParseJSON optionsCampaignRequest
instance ToJSON CampaignRequest where
  toJSON = genericToJSON optionsCampaignRequest

optionsCampaignRequest :: Options
optionsCampaignRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("campaignRequestData", "data")
      , ("campaignRequestEmailUnderscoreaddUnderscoreduplicates", "email_add_duplicates")
      , ("campaignRequestEmailUnderscoreignoreUnderscoremissing", "email_ignore_missing")
      , ("campaignRequestIdUnderscoreignoreUnderscoremissing", "id_ignore_missing")
      , ("campaignRequestRecipients", "recipients")
      , ("campaignRequestEmails", "emails")
      , ("campaignRequestIds", "ids")
      , ("campaignRequestPerUnderscoreuserUnderscoredata", "per_user_data")
      , ("campaignRequestDataUnderscorefileUnderscoreurl", "data_file_url")
      ]


-- | 
data CioId1 = CioId1
  { cioId1CioUnderscoreid :: Maybe Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CioId1 where
  parseJSON = genericParseJSON optionsCioId1
instance ToJSON CioId1 where
  toJSON = genericToJSON optionsCioId1

optionsCioId1 :: Options
optionsCioId1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cioId1CioUnderscoreid", "cio_id")
      ]


-- | 
data CioId2 = CioId2
  { cioId2CioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CioId2 where
  parseJSON = genericParseJSON optionsCioId2
instance ToJSON CioId2 where
  toJSON = genericToJSON optionsCioId2

optionsCioId2 :: Options
optionsCioId2 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cioId2CioUnderscoreid", "cio_id")
      ]


-- | 
data CioId3 = CioId3
  { cioId3CioUnderscoreid :: Maybe Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CioId3 where
  parseJSON = genericParseJSON optionsCioId3
instance ToJSON CioId3 where
  toJSON = genericToJSON optionsCioId3

optionsCioId3 :: Options
optionsCioId3 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cioId3CioUnderscoreid", "cio_id")
      ]


-- | 
data CioId4 = CioId4
  { cioId4CioUnderscoreid :: Text -- ^ A unique, immutable identifier for a person, set by Customer.io when you add a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CioId4 where
  parseJSON = genericParseJSON optionsCioId4
instance ToJSON CioId4 where
  toJSON = genericToJSON optionsCioId4

optionsCioId4 :: Options
optionsCioId4 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cioId4CioUnderscoreid", "cio_id")
      ]


-- | Stores your audience&#39;s subscription preferences if you enable our [subscription center](/subscription-center/) feature. These items are set automatically when people use the unsubscribe link in your messages, but you can set preferences outside the subscription flow. To update select topic preferences while preserving those set for other topics, use JSON dot notation &#x60;\&quot;cio_subscription_preferences.topics.topic_&lt;topic ID&gt;\&quot;:&lt;boolean&gt;&#x60;.
data CioSubscriptionPreferences = CioSubscriptionPreferences
  { cioSubscriptionPreferencesTopics :: Maybe (Map.Map String Bool) -- ^ Contains active topics in your workspace, named `topic_<id>`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CioSubscriptionPreferences where
  parseJSON = genericParseJSON optionsCioSubscriptionPreferences
instance ToJSON CioSubscriptionPreferences where
  toJSON = genericToJSON optionsCioSubscriptionPreferences

optionsCioSubscriptionPreferences :: Options
optionsCioSubscriptionPreferences =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cioSubscriptionPreferencesTopics", "topics")
      ]


-- | 
data CollectionResponse = CollectionResponse
  { collectionResponseBytes :: Maybe Int -- ^ The size of the collection in bytes.
  , collectionResponseCreatedUnderscoreat :: Maybe Int -- ^ The date time when the referenced ID was created.
  , collectionResponseUpdatedUnderscoreat :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , collectionResponseId :: Maybe Int -- ^ The identifier for the collection. This is how you'll reference the collection from the API.
  , collectionResponseName :: Maybe Text -- ^ The name of the collection. This is how you'll reference the collection in liquid, e.g. `{{collection_name.data_property}}`.
  , collectionResponseRows :: Maybe Int -- ^ Represents the number of objects in the `data` array or CSV rows in your collection schema.
  , collectionResponseSchema :: Maybe [Text] -- ^ Lists the top-level keys that you can reference within this collection. Customer.io does not enforce any of these keys as required from your `data`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CollectionResponse where
  parseJSON = genericParseJSON optionsCollectionResponse
instance ToJSON CollectionResponse where
  toJSON = genericToJSON optionsCollectionResponse

optionsCollectionResponse :: Options
optionsCollectionResponse =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("collectionResponseBytes", "bytes")
      , ("collectionResponseCreatedUnderscoreat", "created_at")
      , ("collectionResponseUpdatedUnderscoreat", "updated_at")
      , ("collectionResponseId", "id")
      , ("collectionResponseName", "name")
      , ("collectionResponseRows", "rows")
      , ("collectionResponseSchema", "schema")
      ]


-- | 
data CommonTriggerProps = CommonTriggerProps
  { commonTriggerPropsData :: Maybe (Map.Map String Value) -- ^ Contains information you want to use to populate your broadcast.
  , commonTriggerPropsEmailUnderscoreaddUnderscoreduplicates :: Maybe Bool -- ^ an email address associated with more than one profile id is an error.
  , commonTriggerPropsEmailUnderscoreignoreUnderscoremissing :: Maybe Bool -- ^ If false a missing email address is an error.
  , commonTriggerPropsIdUnderscoreignoreUnderscoremissing :: Maybe Bool -- ^ If false, a missing customer ID is an error.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CommonTriggerProps where
  parseJSON = genericParseJSON optionsCommonTriggerProps
instance ToJSON CommonTriggerProps where
  toJSON = genericToJSON optionsCommonTriggerProps

optionsCommonTriggerProps :: Options
optionsCommonTriggerProps =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("commonTriggerPropsData", "data")
      , ("commonTriggerPropsEmailUnderscoreaddUnderscoreduplicates", "email_add_duplicates")
      , ("commonTriggerPropsEmailUnderscoreignoreUnderscoremissing", "email_ignore_missing")
      , ("commonTriggerPropsIdUnderscoreignoreUnderscoremissing", "id_ignore_missing")
      ]


-- | 
data ComplexAlert = ComplexAlert
  { complexAlertBody :: Maybe Text -- ^ The body of your push notification.
  , complexAlertTitle :: Maybe Text -- ^ The title of your push notification.
  , complexAlertSubtitle :: Maybe Text -- ^ Additional information that explains the purpose of the notification.
  , complexAlertLaunchDashimage :: Maybe Text -- ^ The name of the launch image file you want to display. When a user launches your app, they'll see this image or storyboard file rather than your app’s normal launch image.
  , complexAlertTitleDashlocDashkey :: Maybe Text -- ^ The key for a localized title string in your app’s Localizable.strings files.
  , complexAlertTitleDashlocDashargs :: Maybe [Text] -- ^ An array of replacement value strings for variables in your title string. Each %@ character in the title-loc-key is replaced by a value from this array, in the order they appear in the title string.
  , complexAlertSubtitleDashlocDashkey :: Maybe Text -- ^ The key for a localized subtitle string in your app’s Localizable.strings file.
  , complexAlertSubtitleDashlocDashargs :: Maybe [Text] -- ^ An array of replacement value strings for variables in your subtitle string. Each %@ character in the subtitle-loc-key is replaced by a value from this array, in the order they appear in the subtitle string.
  , complexAlertLocDashkey :: Maybe Text -- ^ The key for a localized message string in your app’s Localizable.strings file.
  , complexAlertLocDashargs :: Maybe [Text] -- ^ An array of replacement value strings for variables in your message text. Each %@ character in the loc-key is replaced by a value from this array, in the order they appear in the message body.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ComplexAlert where
  parseJSON = genericParseJSON optionsComplexAlert
instance ToJSON ComplexAlert where
  toJSON = genericToJSON optionsComplexAlert

optionsComplexAlert :: Options
optionsComplexAlert =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("complexAlertBody", "body")
      , ("complexAlertTitle", "title")
      , ("complexAlertSubtitle", "subtitle")
      , ("complexAlertLaunchDashimage", "launch-image")
      , ("complexAlertTitleDashlocDashkey", "title-loc-key")
      , ("complexAlertTitleDashlocDashargs", "title-loc-args")
      , ("complexAlertSubtitleDashlocDashkey", "subtitle-loc-key")
      , ("complexAlertSubtitleDashlocDashargs", "subtitle-loc-args")
      , ("complexAlertLocDashkey", "loc-key")
      , ("complexAlertLocDashargs", "loc-args")
      ]


-- | When filtering for people, you can use &#x60;and&#x60; and &#x60;or&#x60; arrays to determine the logic for a group of filter conditions. &#x60;not&#x60; reverses the filter condition and matches when the condition is false. &#x60;segment&#x60; and &#x60;attribute&#x60; represent the individual conditions you can filter a group of people for.
data ComplexAudienceFilter = ComplexAudienceFilter
  { complexAudienceFilterAnd :: Maybe [ComplexAudienceFilterAndInner] -- ^ Returns results matching *all* conditions.
  , complexAudienceFilterOr :: Maybe [ComplexAudienceFilterAndInner] -- ^ Returns results matching *any* conditions.
  , complexAudienceFilterNot :: Maybe ComplexAudienceFilterNot -- ^ 
  , complexAudienceFilterSegment :: Maybe Segment -- ^ 
  , complexAudienceFilterAttribute :: Maybe Attribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ComplexAudienceFilter where
  parseJSON = genericParseJSON optionsComplexAudienceFilter
instance ToJSON ComplexAudienceFilter where
  toJSON = genericToJSON optionsComplexAudienceFilter

optionsComplexAudienceFilter :: Options
optionsComplexAudienceFilter =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("complexAudienceFilterAnd", "and")
      , ("complexAudienceFilterOr", "or")
      , ("complexAudienceFilterNot", "not")
      , ("complexAudienceFilterSegment", "segment")
      , ("complexAudienceFilterAttribute", "attribute")
      ]


-- | 
data ComplexAudienceFilterAndInner = ComplexAudienceFilterAndInner
  { complexAudienceFilterAndInnerSegment :: Maybe Segment -- ^ 
  , complexAudienceFilterAndInnerAttribute :: Maybe Attribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ComplexAudienceFilterAndInner where
  parseJSON = genericParseJSON optionsComplexAudienceFilterAndInner
instance ToJSON ComplexAudienceFilterAndInner where
  toJSON = genericToJSON optionsComplexAudienceFilterAndInner

optionsComplexAudienceFilterAndInner :: Options
optionsComplexAudienceFilterAndInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("complexAudienceFilterAndInnerSegment", "segment")
      , ("complexAudienceFilterAndInnerAttribute", "attribute")
      ]


-- | Returns results if a condition is false. While and/or support an array of items, &#x60;not&#x60; supports a single filter object.
data ComplexAudienceFilterNot = ComplexAudienceFilterNot
  { complexAudienceFilterNotAnd :: Maybe [ComplexAudienceFilterAndInner] -- ^ Match *all* conditions to return results.
  , complexAudienceFilterNotOr :: Maybe [ComplexAudienceFilterAndInner] -- ^ Match *any* condition to return results.
  , complexAudienceFilterNotSegment :: Maybe Segment -- ^ 
  , complexAudienceFilterNotAttribute :: Maybe Attribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ComplexAudienceFilterNot where
  parseJSON = genericParseJSON optionsComplexAudienceFilterNot
instance ToJSON ComplexAudienceFilterNot where
  toJSON = genericToJSON optionsComplexAudienceFilterNot

optionsComplexAudienceFilterNot :: Options
optionsComplexAudienceFilterNot =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("complexAudienceFilterNotAnd", "and")
      , ("complexAudienceFilterNotOr", "or")
      , ("complexAudienceFilterNotSegment", "segment")
      , ("complexAudienceFilterNotAttribute", "attribute")
      ]


-- | Set a true/false condition determining which content to show. Use a variable in your condition to populate the condition when you set up your message in Customer.io.
data ConditionalWidget = ConditionalWidget
  { conditionalWidgetType :: Maybe Text -- ^ Defines the widget type.
  , conditionalWidgetCondition :: Maybe Text -- ^ The condition you want to evaluate. You can evaluate a condition using `>`, `<`, `==` & `in` operators. If you don't use an operator, the condition checks if the property is null.
  , conditionalWidgetTrue :: Maybe Value -- ^ The component you want to show when your condition is true.
  , conditionalWidgetFalse :: Maybe Value -- ^ The component you want to show when your condition is false.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ConditionalWidget where
  parseJSON = genericParseJSON optionsConditionalWidget
instance ToJSON ConditionalWidget where
  toJSON = genericToJSON optionsConditionalWidget

optionsConditionalWidget :: Options
optionsConditionalWidget =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("conditionalWidgetType", "type")
      , ("conditionalWidgetCondition", "condition")
      , ("conditionalWidgetTrue", "true")
      , ("conditionalWidgetFalse", "false")
      ]


-- | 
data ContentObject = ContentObject
  { contentObjectId :: Maybe Int -- ^ The identifier of a newsletter variant—a language in a multi-language newsletter or a test in an A/B test.
  , contentObjectNewsletterUnderscoreid :: Maybe Int -- ^ The identifier for a newsletter.
  , contentObjectDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , contentObjectName :: Maybe Text -- ^ The name of the variant, if it exists.
  , contentObjectLayout :: Maybe Text -- ^ The layout used for the variant, if it exists.
  , contentObjectBody :: Maybe Text -- ^ The body of the variant. You cannot modify the body if you created it with our drag-and-drop editor.
  , contentObjectBodyUnderscoreamp :: Maybe Text -- ^ If your message is an email, this is the AMP-enabled body of your message. If your recipient's email client doesn't support AMP, the `body` represents your fallback message.
  , contentObjectLanguage :: Maybe Text -- ^ The language variant for your message. If you don't use our [localization feature](/localization), or this is the default message, this value is an empty string.
  , contentObjectType :: Maybe Text -- ^ The type of message/action.
  , contentObjectFrom :: Maybe Text -- ^ The address that the message is from, relevant if the action `type` is `email`.
  , contentObjectFromUnderscoreid :: Maybe Int -- ^ The identifier of the `from` address, commonly known as the \"sender\". You can [list your sender identities](#operation/listSenders) to match the ID to a specific address.
  , contentObjectReplyUnderscoreto :: Maybe Text -- ^ The address that receives replies for the message, if applicable.
  , contentObjectReplyUnderscoretoUnderscoreid :: Maybe Int -- ^ The identifier for the `reply_to` address, if applicable. You can [list your sender identities](#operation/listSenders) to match the ID to a specific address.
  , contentObjectPreprocessor :: Maybe Text -- ^ By default, we process CSS before emails leave Customer.io using Premailer. If your message included CSS and pre-processing is not disabled, this key indicates the pre-processor.
  , contentObjectRecipient :: Maybe Text -- ^ The recipient address for an action.
  , contentObjectSubject :: Maybe Text -- ^ The subject line for an `email` action.
  , contentObjectBcc :: Maybe Text -- ^ The blind-copy address(es) for this action.
  , contentObjectFakeUnderscorebcc :: Maybe Bool -- ^ If true, rather than sending true copies to BCC addresses, Customer.io sends a copy of the message with the subject line containing the recipient address(es). 
  , contentObjectPreheaderUnderscoretext :: Maybe Text -- ^ Also known as \"preview text\", this specifies the small block of text shown in an end-user's email inbox, next to, or underneath, the subject line.
  , contentObjectHeaders :: Maybe (Map.Map String Text) -- ^ An object containing headers, where the key is the header name and the value is the header value. Header names and values must be strings and cannot contain any non-ASCII characters or empty spaces. Some headers are reserved and cannot be overwritten.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ContentObject where
  parseJSON = genericParseJSON optionsContentObject
instance ToJSON ContentObject where
  toJSON = genericToJSON optionsContentObject

optionsContentObject :: Options
optionsContentObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("contentObjectId", "id")
      , ("contentObjectNewsletterUnderscoreid", "newsletter_id")
      , ("contentObjectDeduplicateUnderscoreid", "deduplicate_id")
      , ("contentObjectName", "name")
      , ("contentObjectLayout", "layout")
      , ("contentObjectBody", "body")
      , ("contentObjectBodyUnderscoreamp", "body_amp")
      , ("contentObjectLanguage", "language")
      , ("contentObjectType", "type")
      , ("contentObjectFrom", "from")
      , ("contentObjectFromUnderscoreid", "from_id")
      , ("contentObjectReplyUnderscoreto", "reply_to")
      , ("contentObjectReplyUnderscoretoUnderscoreid", "reply_to_id")
      , ("contentObjectPreprocessor", "preprocessor")
      , ("contentObjectRecipient", "recipient")
      , ("contentObjectSubject", "subject")
      , ("contentObjectBcc", "bcc")
      , ("contentObjectFakeUnderscorebcc", "fake_bcc")
      , ("contentObjectPreheaderUnderscoretext", "preheader_text")
      , ("contentObjectHeaders", "headers")
      ]


-- | 
data CriticalAlert = CriticalAlert
  { criticalAlertCritical :: Maybe Int -- ^ 1 indicates critical. 0 is not critical.
  , criticalAlertName :: Maybe Text -- ^ The name of a sound file in your app’s main bundle or in the Library/Sounds folder of your app’s container directory. Use “default” to play the system sound.
  , criticalAlertVolume :: Maybe Double -- ^ The volume for a critical alert between 0 and 1, where 0 is silent and 1 is full volume.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CriticalAlert where
  parseJSON = genericParseJSON optionsCriticalAlert
instance ToJSON CriticalAlert where
  toJSON = genericToJSON optionsCriticalAlert

optionsCriticalAlert :: Options
optionsCriticalAlert =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("criticalAlertCritical", "critical")
      , ("criticalAlertName", "name")
      , ("criticalAlertVolume", "volume")
      ]


-- | Properties supported specifically by Android on FCM.
data CustomAndroidProps = CustomAndroidProps
  { customAndroidPropsIcon :: Maybe Text -- ^ Sets the notification icon to `myicon` for drawable resource `myicon`. If you don't send this key, FCM displays the launcher icon from your app manifest.
  , customAndroidPropsSound :: Maybe Text -- ^ The sound that plays when the device receives the notification. Supports `\"default\"` or the filename of a sound resource bundled in your app. Sound files must reside in `/res/raw/`.
  , customAndroidPropsTag :: Maybe Text -- ^ Identifier to replace existing notifications in the notification drawer. If empty, each request creates a new notification.  If you specify a tag, and a notification with the same tag is already being shown, the new notification replaces the existing one in the notification drawer.  
  , customAndroidPropsColor :: Maybe Text -- ^ The notification's icon color in `#rrggbb` format.
  , customAndroidPropsClickUnderscoreaction :: Maybe Text -- ^ The action that occurs when a user taps on the notification. Launches an activity with a matching intent filter when a person taps the notification.
  , customAndroidPropsBodyUnderscorelocUnderscorekey :: Maybe Text -- ^ The key to the body string in the app's string resources that you want to use to localize the body text to the user's current localization. See [String Resources](https://developer.android.com/guide/topics/resources/string-resource/) for more information.
  , customAndroidPropsBodyUnderscorelocUnderscorearg :: Maybe Text -- ^ Variable string values used in place of the format specifiers in `body_loc_key` to localize the body text to the user's current localization. See Formatting and Styling for more information.
  , customAndroidPropsTitleUnderscorelocUnderscorekey :: Maybe Text -- ^ The key to the title string in the app's string resources that you want to use to localize the title text to the user's current localization. See [String Resources](https://developer.android.com/guide/topics/resources/string-resource/) for more information.
  , customAndroidPropsTitleUnderscorelocUnderscorearg :: Maybe Text -- ^ Variable string values used in place of the format specifiers in `title_loc_key` to localize the title text to the user's current localization. See Formatting and Styling for more information.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CustomAndroidProps where
  parseJSON = genericParseJSON optionsCustomAndroidProps
instance ToJSON CustomAndroidProps where
  toJSON = genericToJSON optionsCustomAndroidProps

optionsCustomAndroidProps :: Options
optionsCustomAndroidProps =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("customAndroidPropsIcon", "icon")
      , ("customAndroidPropsSound", "sound")
      , ("customAndroidPropsTag", "tag")
      , ("customAndroidPropsColor", "color")
      , ("customAndroidPropsClickUnderscoreaction", "click_action")
      , ("customAndroidPropsBodyUnderscorelocUnderscorekey", "body_loc_key")
      , ("customAndroidPropsBodyUnderscorelocUnderscorearg", "body_loc_arg")
      , ("customAndroidPropsTitleUnderscorelocUnderscorekey", "title_loc_key")
      , ("customAndroidPropsTitleUnderscorelocUnderscorearg", "title_loc_arg")
      ]


-- | 
data CustomIntegration = CustomIntegration
  { customIntegrationMessage :: TransactionalSharedPushObjectCustomPayloadAndroid -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CustomIntegration where
  parseJSON = genericParseJSON optionsCustomIntegration
instance ToJSON CustomIntegration where
  toJSON = genericToJSON optionsCustomIntegration

optionsCustomIntegration :: Options
optionsCustomIntegration =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("customIntegrationMessage", "message")
      ]


-- | Send your broadcast to a group of people defined by a set of filters.
data CustomRecipients = CustomRecipients
  { customRecipientsRecipients :: AudienceFilter -- ^ 
  , customRecipientsData :: Maybe (Map.Map String Value) -- ^ Contains information you want to use to populate your broadcast.
  , customRecipientsEmailUnderscoreaddUnderscoreduplicates :: Maybe Bool -- ^ an email address associated with more than one profile id is an error.
  , customRecipientsEmailUnderscoreignoreUnderscoremissing :: Maybe Bool -- ^ If false a missing email address is an error.
  , customRecipientsIdUnderscoreignoreUnderscoremissing :: Maybe Bool -- ^ If false, a missing customer ID is an error.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CustomRecipients where
  parseJSON = genericParseJSON optionsCustomRecipients
instance ToJSON CustomRecipients where
  toJSON = genericToJSON optionsCustomRecipients

optionsCustomRecipients :: Options
optionsCustomRecipients =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("customRecipientsRecipients", "recipients")
      , ("customRecipientsData", "data")
      , ("customRecipientsEmailUnderscoreaddUnderscoreduplicates", "email_add_duplicates")
      , ("customRecipientsEmailUnderscoreignoreUnderscoremissing", "email_ignore_missing")
      , ("customRecipientsIdUnderscoreignoreUnderscoremissing", "id_ignore_missing")
      ]


-- | 
data CustomerAttributesObject = CustomerAttributesObject
  { customerAttributesObjectCustomer :: Maybe CustomerAttributesObjectCustomer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CustomerAttributesObject where
  parseJSON = genericParseJSON optionsCustomerAttributesObject
instance ToJSON CustomerAttributesObject where
  toJSON = genericToJSON optionsCustomerAttributesObject

optionsCustomerAttributesObject :: Options
optionsCustomerAttributesObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("customerAttributesObjectCustomer", "customer")
      ]


-- | The profile you referenced by ID in the request.
data CustomerAttributesObjectCustomer = CustomerAttributesObjectCustomer
  { customerAttributesObjectCustomerId :: Maybe Text -- ^ The customer ID referenced in the request.
  , customerAttributesObjectCustomerIdentifiers :: Maybe ActivityObjectCustomerIdentifiers -- ^ 
  , customerAttributesObjectCustomerAttributes :: Maybe CustomerAttributesObjectCustomerAttributes -- ^ 
  , customerAttributesObjectCustomerTimestamps :: Maybe CustomerAttributesObjectCustomerTimestamps -- ^ 
  , customerAttributesObjectCustomerUnsubscribed :: Maybe Bool -- ^ If true, the person is unsubscribed from messages.
  , customerAttributesObjectCustomerDevices :: Maybe [AddDeviceRequestDevice] -- ^ Lists the devices associated with the customer profile.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CustomerAttributesObjectCustomer where
  parseJSON = genericParseJSON optionsCustomerAttributesObjectCustomer
instance ToJSON CustomerAttributesObjectCustomer where
  toJSON = genericToJSON optionsCustomerAttributesObjectCustomer

optionsCustomerAttributesObjectCustomer :: Options
optionsCustomerAttributesObjectCustomer =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("customerAttributesObjectCustomerId", "id")
      , ("customerAttributesObjectCustomerIdentifiers", "identifiers")
      , ("customerAttributesObjectCustomerAttributes", "attributes")
      , ("customerAttributesObjectCustomerTimestamps", "timestamps")
      , ("customerAttributesObjectCustomerUnsubscribed", "unsubscribed")
      , ("customerAttributesObjectCustomerDevices", "devices")
      ]


-- | Contains attributes assigned to this profile, including your workspace&#39;s people-identifiers (by default, these are &#x60;id&#x60;, &#x60;email&#x60;, and &#x60;cio_id&#x60;). Attributes are all stored as strings.
newtype CustomerAttributesObjectCustomerAttributes = CustomerAttributesObjectCustomerAttributes { unCustomerAttributesObjectCustomerAttributes :: (Map.Map Text Text) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | The epoch timestamps when corresponding attributes were set on the customer profile.
data CustomerAttributesObjectCustomerTimestamps = CustomerAttributesObjectCustomerTimestamps
  { customerAttributesObjectCustomerTimestampsCioUnderscoreid :: Maybe Int -- ^ The date-time when `cio_id` was assigned.
  , customerAttributesObjectCustomerTimestampsUnderscorelastUnderscoreemailed :: Maybe Int -- ^ The date-time when you last emailed a person.
  , customerAttributesObjectCustomerTimestampsEmail :: Maybe Int -- ^ The date-time when the person's email address was added.
  , customerAttributesObjectCustomerTimestampsId :: Maybe Int -- ^ The date-time when the person's ID was generated.
  , customerAttributesObjectCustomerTimestampsUnsubscribed :: Maybe Int -- ^ The date-time when the person unsubscribed.
  , customerAttributesObjectCustomerTimestampsUnderscorecioUnderscoresubscriptionUnderscorepreferencesUnderscorecomputed :: Maybe Int -- ^ Because this value is computed and not actually set on a person, this value is always 0
  , customerAttributesObjectCustomerTimestampsCioUnderscoresubscriptionUnderscorepreferences :: Maybe Int -- ^ The datetime when a person's subscription preferences were last updated.
  , customerAttributesObjectCustomerTimestampsAdditionalProperties :: Maybe Value -- ^ Timestamps when attributes assigned to the person were set.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CustomerAttributesObjectCustomerTimestamps where
  parseJSON = genericParseJSON optionsCustomerAttributesObjectCustomerTimestamps
instance ToJSON CustomerAttributesObjectCustomerTimestamps where
  toJSON = genericToJSON optionsCustomerAttributesObjectCustomerTimestamps

optionsCustomerAttributesObjectCustomerTimestamps :: Options
optionsCustomerAttributesObjectCustomerTimestamps =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("customerAttributesObjectCustomerTimestampsCioUnderscoreid", "cio_id")
      , ("customerAttributesObjectCustomerTimestampsUnderscorelastUnderscoreemailed", "_last_emailed")
      , ("customerAttributesObjectCustomerTimestampsEmail", "email")
      , ("customerAttributesObjectCustomerTimestampsId", "id")
      , ("customerAttributesObjectCustomerTimestampsUnsubscribed", "unsubscribed")
      , ("customerAttributesObjectCustomerTimestampsUnderscorecioUnderscoresubscriptionUnderscorepreferencesUnderscorecomputed", "_cio_subscription_preferences_computed")
      , ("customerAttributesObjectCustomerTimestampsCioUnderscoresubscriptionUnderscorepreferences", "cio_subscription_preferences")
      , ("customerAttributesObjectCustomerTimestampsAdditionalProperties", "additionalProperties")
      ]


-- | Describes the customer events reported from Customer.io to a webhook.
data CustomerEvents = CustomerEvents
  { customerEventsCustomerUnderscoresubscribed :: Maybe Bool -- ^ A person's `unsubscribed` attribute was explicitly set to `false`. Set to true to report `subscribed` events.
  , customerEventsCustomerUnderscoreunsubscribed :: Maybe Bool -- ^ A person's `unsubscribed` attribute was explicitly set to `true`. Set to true to report `unsubscribed` events.
  , customerEventsCustomerUnderscoresubscriptionUnderscorepreferencesUnderscorechanged :: Maybe Bool -- ^ A person's subscription preferences changed. [Learn more about the subscription center](/subscription-center/).
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CustomerEvents where
  parseJSON = genericParseJSON optionsCustomerEvents
instance ToJSON CustomerEvents where
  toJSON = genericToJSON optionsCustomerEvents

optionsCustomerEvents :: Options
optionsCustomerEvents =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("customerEventsCustomerUnderscoresubscribed", "customer_subscribed")
      , ("customerEventsCustomerUnderscoreunsubscribed", "customer_unsubscribed")
      , ("customerEventsCustomerUnderscoresubscriptionUnderscorepreferencesUnderscorechanged", "customer_subscription_preferences_changed")
      ]


-- | Contains identifiers for the person represented in a response—&#x60;id&#x60;, &#x60;cio_id&#x60;, and &#x60;email&#x60; (if your workspace uses the *Email or ID* setting). If the person&#39;s &#x60;id&#x60; or &#x60;email&#x60; is not set, the value will be null.  We recommend that you use this object rather than the less descriptive &#x60;customer_id&#x60;. 
data CustomerIdentifiers = CustomerIdentifiers
  { customerIdentifiersEmail :: Text -- ^ A person's email address, if set.
  , customerIdentifiersId :: Text -- ^ A person's unique ID, if set. This is the same as the `customer_id` if present.
  , customerIdentifiersCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CustomerIdentifiers where
  parseJSON = genericParseJSON optionsCustomerIdentifiers
instance ToJSON CustomerIdentifiers where
  toJSON = genericToJSON optionsCustomerIdentifiers

optionsCustomerIdentifiers :: Options
optionsCustomerIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("customerIdentifiersEmail", "email")
      , ("customerIdentifiersId", "id")
      , ("customerIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | The URL of a data file containing per-user data, each line containing a json map with either &#x60;id&#x60; and &#x60;data&#x60; or &#x60;email&#x60; and &#x60;data&#x60; keys. **When you trigger a broadcast, the people represented by &#x60;id&#x60; and &#x60;email&#x60; must already exist in your workspace.** Requests to trigger a broadcast cannot create new people. 
data DataFileURL = DataFileURL
  { dataFileURLDataUnderscorefileUnderscoreurl :: Text -- ^ The URL of a data file containing per-user data. Each line is a single object representing a person: either a json map of `id` and `data` or `email` and `data` keys. For example, `{\"email\":\"road@runner.net\",\"data\":{\"voucher_code\": \"coyote\"}}`.  The IDs or emails in your data file must already exist in your workspace. Your broadcast cannot add new people to your workspace and won't send messages for IDs or email addresses that don't match a person in your workspace. 
  , dataFileURLData :: Maybe (Map.Map String Value) -- ^ Contains information you want to use to populate your broadcast.
  , dataFileURLEmailUnderscoreaddUnderscoreduplicates :: Maybe Bool -- ^ an email address associated with more than one profile id is an error.
  , dataFileURLEmailUnderscoreignoreUnderscoremissing :: Maybe Bool -- ^ If false a missing email address is an error.
  , dataFileURLIdUnderscoreignoreUnderscoremissing :: Maybe Bool -- ^ If false, a missing customer ID is an error.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataFileURL where
  parseJSON = genericParseJSON optionsDataFileURL
instance ToJSON DataFileURL where
  toJSON = genericToJSON optionsDataFileURL

optionsDataFileURL :: Options
optionsDataFileURL =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("dataFileURLDataUnderscorefileUnderscoreurl", "data_file_url")
      , ("dataFileURLData", "data")
      , ("dataFileURLEmailUnderscoreaddUnderscoreduplicates", "email_add_duplicates")
      , ("dataFileURLEmailUnderscoreignoreUnderscoremissing", "email_ignore_missing")
      , ("dataFileURLIdUnderscoreignoreUnderscoremissing", "id_ignore_missing")
      ]


-- | 
data DataObjectOnly = DataObjectOnly
  { dataObjectOnlyData :: DataObjectOnlyData -- ^ 
  , dataObjectOnlyAndroid :: Maybe DataObjectOnlyAndroid -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataObjectOnly where
  parseJSON = genericParseJSON optionsDataObjectOnly
instance ToJSON DataObjectOnly where
  toJSON = genericToJSON optionsDataObjectOnly

optionsDataObjectOnly :: Options
optionsDataObjectOnly =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("dataObjectOnlyData", "data")
      , ("dataObjectOnlyAndroid", "android")
      ]


-- | Contains properties that are **not** interpreted by the SDK but are defined by FCM. You need to write your own code to handle these Android push features.
data DataObjectOnlyAndroid = DataObjectOnlyAndroid
  { dataObjectOnlyAndroidNotification :: Maybe TransactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotification -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataObjectOnlyAndroid where
  parseJSON = genericParseJSON optionsDataObjectOnlyAndroid
instance ToJSON DataObjectOnlyAndroid where
  toJSON = genericToJSON optionsDataObjectOnlyAndroid

optionsDataObjectOnlyAndroid :: Options
optionsDataObjectOnlyAndroid =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("dataObjectOnlyAndroidNotification", "notification")
      ]


-- | Contains all properties interpreted by the SDK.
data DataObjectOnlyData = DataObjectOnlyData
  { dataObjectOnlyDataTitle :: Maybe Text -- ^ The title of your push notification.
  , dataObjectOnlyDataBody :: Maybe Text -- ^ The body of your push notification.
  , dataObjectOnlyDataImage :: Maybe Text -- ^ The URL of an HTTPS image that you want to use for your message.
  , dataObjectOnlyDataLink :: Maybe Text -- ^ A deep link (to a page in your app), or a link to a web page.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataObjectOnlyData where
  parseJSON = genericParseJSON optionsDataObjectOnlyData
instance ToJSON DataObjectOnlyData where
  toJSON = genericToJSON optionsDataObjectOnlyData

optionsDataObjectOnlyData :: Options
optionsDataObjectOnlyData =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("dataObjectOnlyDataTitle", "title")
      , ("dataObjectOnlyDataBody", "body")
      , ("dataObjectOnlyDataImage", "image")
      , ("dataObjectOnlyDataLink", "link")
      ]


-- | Determines whether your import operation performs &#x60;all&#x60; add/update operations, only adds items (&#x60;only_new&#x60;), or only updates existing items (&#x60;only_existing&#x60;). Defaults to &#x60;all&#x60;. If &#x60;import_type&#x60; is &#x60;event&#x60;, you can only use &#x60;all&#x60; or &#x60;only_existing&#x60;.   This field was previously called &#x60;people_to_process&#x60; - we still support it but will deprecate it soon. 
data DataToProcess = DataToProcess
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataToProcess where
  parseJSON = genericParseJSON optionsDataToProcess
instance ToJSON DataToProcess where
  toJSON = genericToJSON optionsDataToProcess

optionsDataToProcess :: Options
optionsDataToProcess =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data Date = Date
  { dateId :: Maybe Int -- ^ The identifier for a campaign.
  , dateDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , dateName :: Maybe Text -- ^ The name of the campaign.
  , dateType :: Maybe Text -- ^ The type of campaign trigger. **Sunsetting on March 30, 2025**
  , dateCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , dateUpdated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , dateActive :: Maybe Bool -- ^ If true, the campaign is active and can still send messages.
  , dateState :: Maybe Text -- ^ The status of the campaign.
  , dateActions :: Maybe [SegmentActionsInner] -- ^ An array of actions contained within the campaign.
  , dateFirstUnderscorestarted :: Maybe Int -- ^ The date and time when you first started the campaign and it first became eligible to be triggered.
  , dateTags :: Maybe [Text] -- ^ An array of tags you set on this campaign.
  , dateFilterUnderscoresegmentUnderscoreids :: Maybe [Int] -- ^ A list of segments used in the campaign filter, returned if the campaign audience was filtered on one or more segments.
  , dateFrequency :: Maybe Text -- ^ How often a person will receive this campaign based on the date specified in the campaign trigger.
  , dateDateUnderscoreattribute :: Maybe Text -- ^ The attribute on people's profiles you use to configure the date of the campaign trigger.
  , dateTimezone :: Maybe Text -- ^ The timezone you set to configure the date of the campaign trigger.
  , dateUseUnderscorecustomerUnderscoretimezone :: Maybe Bool -- ^ If you chose \"the user's timezone\" while configuring the date of the campaign trigger, this is `true`. Otherwise, you set a specific timezone so it's `false`.
  , dateStartUnderscorehour :: Maybe Int -- ^ The hour you set the campaign to trigger. Follows the 24-hour clock.
  , dateStartUnderscoreminutes :: Maybe Int -- ^ The minutes you set the campaign to trigger. Follows the 24-hour clock.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Date where
  parseJSON = genericParseJSON optionsDate
instance ToJSON Date where
  toJSON = genericToJSON optionsDate

optionsDate :: Options
optionsDate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("dateId", "id")
      , ("dateDeduplicateUnderscoreid", "deduplicate_id")
      , ("dateName", "name")
      , ("dateType", "type")
      , ("dateCreated", "created")
      , ("dateUpdated", "updated")
      , ("dateActive", "active")
      , ("dateState", "state")
      , ("dateActions", "actions")
      , ("dateFirstUnderscorestarted", "first_started")
      , ("dateTags", "tags")
      , ("dateFilterUnderscoresegmentUnderscoreids", "filter_segment_ids")
      , ("dateFrequency", "frequency")
      , ("dateDateUnderscoreattribute", "date_attribute")
      , ("dateTimezone", "timezone")
      , ("dateUseUnderscorecustomerUnderscoretimezone", "use_customer_timezone")
      , ("dateStartUnderscorehour", "start_hour")
      , ("dateStartUnderscoreminutes", "start_minutes")
      ]


-- | Send your broadcast to the default set of recipients defined in the UI.
data DefaultAudience = DefaultAudience
  { defaultAudienceData :: Maybe (Map.Map String Value) -- ^ Contains information you want to use to populate your broadcast.
  , defaultAudienceEmailUnderscoreaddUnderscoreduplicates :: Maybe Bool -- ^ an email address associated with more than one profile id is an error.
  , defaultAudienceEmailUnderscoreignoreUnderscoremissing :: Maybe Bool -- ^ If false a missing email address is an error.
  , defaultAudienceIdUnderscoreignoreUnderscoremissing :: Maybe Bool -- ^ If false, a missing customer ID is an error.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DefaultAudience where
  parseJSON = genericParseJSON optionsDefaultAudience
instance ToJSON DefaultAudience where
  toJSON = genericToJSON optionsDefaultAudience

optionsDefaultAudience :: Options
optionsDefaultAudience =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("defaultAudienceData", "data")
      , ("defaultAudienceEmailUnderscoreaddUnderscoreduplicates", "email_add_duplicates")
      , ("defaultAudienceEmailUnderscoreignoreUnderscoremissing", "email_ignore_missing")
      , ("defaultAudienceIdUnderscoreignoreUnderscoremissing", "id_ignore_missing")
      ]


-- | Delete a person from your workspace.
data Delete = Delete
  { deleteType :: Text -- ^ The operation modifies a person in Customer.io
  , deleteIdentifiers :: IdentifyAllOfIdentifiers -- ^ 
  , deleteAction :: Text -- ^ Indicates that the operation will `delete` the the item of the specified `type`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Delete where
  parseJSON = genericParseJSON optionsDelete
instance ToJSON Delete where
  toJSON = genericToJSON optionsDelete

optionsDelete :: Options
optionsDelete =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("deleteType", "type")
      , ("deleteIdentifiers", "identifiers")
      , ("deleteAction", "action")
      ]


-- | Delete an object. This also removes relationships from people. 
data Delete1 = Delete1
  { delete1Identifiers :: Maybe IdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers -- ^ 
  , delete1Type :: Text -- ^ The operation modifies a single object—non person data.
  , delete1Action :: Text -- ^ Indicates that the operation will `delete` the the item of the specified `type`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Delete1 where
  parseJSON = genericParseJSON optionsDelete1
instance ToJSON Delete1 where
  toJSON = genericToJSON optionsDelete1

optionsDelete1 :: Options
optionsDelete1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("delete1Identifiers", "identifiers")
      , ("delete1Type", "type")
      , ("delete1Action", "action")
      ]


-- | Delete devices that belong to a person.
data DeleteDevice = DeleteDevice
  { deleteDeviceType :: Text -- ^ The operation modifies a person in Customer.io
  , deleteDeviceIdentifiers :: IdentifyAllOfIdentifiers -- ^ 
  , deleteDeviceAction :: Text -- ^ Delete a device from a person's profile.
  , deleteDeviceDevice :: DeleteDeviceAllOfDevice -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeleteDevice where
  parseJSON = genericParseJSON optionsDeleteDevice
instance ToJSON DeleteDevice where
  toJSON = genericToJSON optionsDeleteDevice

optionsDeleteDevice :: Options
optionsDeleteDevice =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("deleteDeviceType", "type")
      , ("deleteDeviceIdentifiers", "identifiers")
      , ("deleteDeviceAction", "action")
      , ("deleteDeviceDevice", "device")
      ]


-- | The device you want to remove.
data DeleteDeviceAllOfDevice = DeleteDeviceAllOfDevice
  { deleteDeviceAllOfDeviceToken :: Text -- ^ The token of the device you want to remove.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeleteDeviceAllOfDevice where
  parseJSON = genericParseJSON optionsDeleteDeviceAllOfDevice
instance ToJSON DeleteDeviceAllOfDevice where
  toJSON = genericToJSON optionsDeleteDeviceAllOfDevice

optionsDeleteDeviceAllOfDevice :: Options
optionsDeleteDeviceAllOfDevice =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("deleteDeviceAllOfDeviceToken", "token")
      ]


-- | Remove multiple object relationships from a person.
data DeleteRelationships = DeleteRelationships
  { deleteRelationshipsType :: Text -- ^ The operation modifies a person in Customer.io
  , deleteRelationshipsIdentifiers :: IdentifyAllOfIdentifiers -- ^ 
  , deleteRelationshipsAction :: Text -- ^ This operation deletes an object relationship from one or more people.
  , deleteRelationshipsCioUnderscorerelationships :: [IdentifyRequestCioRelationshipsRelationshipsInner] -- ^ Each object in the array represents a relationship you want to add to, or remove from, a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeleteRelationships where
  parseJSON = genericParseJSON optionsDeleteRelationships
instance ToJSON DeleteRelationships where
  toJSON = genericToJSON optionsDeleteRelationships

optionsDeleteRelationships :: Options
optionsDeleteRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("deleteRelationshipsType", "type")
      , ("deleteRelationshipsIdentifiers", "identifiers")
      , ("deleteRelationshipsAction", "action")
      , ("deleteRelationshipsCioUnderscorerelationships", "cio_relationships")
      ]


-- | Delete relationships between an object and one or more people.
data DeleteRelationships1 = DeleteRelationships1
  { deleteRelationships1Identifiers :: Maybe IdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers -- ^ 
  , deleteRelationships1Type :: Text -- ^ The operation modifies a single object—non person data.
  , deleteRelationships1Action :: Text -- ^ This operation deletes an object relationship from one or more people.
  , deleteRelationships1CioUnderscorerelationships :: [Identify1AllOfCioRelationshipsInner] -- ^ The people you want to associate with an object. Each object in the array represents a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeleteRelationships1 where
  parseJSON = genericParseJSON optionsDeleteRelationships1
instance ToJSON DeleteRelationships1 where
  toJSON = genericToJSON optionsDeleteRelationships1

optionsDeleteRelationships1 :: Options
optionsDeleteRelationships1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("deleteRelationships1Identifiers", "identifiers")
      , ("deleteRelationships1Type", "type")
      , ("deleteRelationships1Action", "action")
      , ("deleteRelationships1CioUnderscorerelationships", "cio_relationships")
      ]


-- | The \&quot;delivery\&quot; type lets you attribute metrics to messages that don&#39;t self-report back to Customer.io, like push and in-app notifications.
data Delivery = Delivery
  { deliveryType :: Text -- ^ The \"delivery\" type lets you attribute metrics to messages that don't self-report back to Customer.io, like push and in-app notifications.
  , deliveryAction :: Text -- ^ An `event` action indicates a delivery event. Use the `name` to determine the specific metric that you want to attribute to this delivery.
  , deliveryIdentifiers :: DeliveryIdentifiers -- ^ 
  , deliveryName :: Text -- ^ The name of the metric you want to attribute to this \"delivery\".
  , deliveryAttributes :: DeliveryAttributes -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Delivery where
  parseJSON = genericParseJSON optionsDelivery
instance ToJSON Delivery where
  toJSON = genericToJSON optionsDelivery

optionsDelivery :: Options
optionsDelivery =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("deliveryType", "type")
      , ("deliveryAction", "action")
      , ("deliveryIdentifiers", "identifiers")
      , ("deliveryName", "name")
      , ("deliveryAttributes", "attributes")
      ]


-- | Contains information about the delivery and the individual who received the message.
data DeliveryAttributes = DeliveryAttributes
  { deliveryAttributesDeviceUnderscoretoken :: Text -- ^ The device that received the message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeliveryAttributes where
  parseJSON = genericParseJSON optionsDeliveryAttributes
instance ToJSON DeliveryAttributes where
  toJSON = genericToJSON optionsDeliveryAttributes

optionsDeliveryAttributes :: Options
optionsDeliveryAttributes =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("deliveryAttributesDeviceUnderscoretoken", "device_token")
      ]


-- | Contains identifiers for the delivery itself.
data DeliveryIdentifiers = DeliveryIdentifiers
  { deliveryIdentifiersId :: Maybe Text -- ^ The `delivery_id` for the delivery that you want to attribute metrics to.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeliveryIdentifiers where
  parseJSON = genericParseJSON optionsDeliveryIdentifiers
instance ToJSON DeliveryIdentifiers where
  toJSON = genericToJSON optionsDeliveryIdentifiers

optionsDeliveryIdentifiers :: Options
optionsDeliveryIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("deliveryIdentifiersId", "id")
      ]


-- | The \&quot;delivery\&quot; type lets you attribute metrics to messages that don&#39;t self-report back to Customer.io, like push and in-app notifications.
data DeliveryOperations = DeliveryOperations
  { deliveryOperationsType :: Text -- ^ The \"delivery\" type lets you attribute metrics to messages that don't self-report back to Customer.io, like push and in-app notifications.
  , deliveryOperationsAction :: Text -- ^ An `event` action indicates a delivery event. Use the `name` to determine the specific metric that you want to attribute to this delivery.
  , deliveryOperationsIdentifiers :: DeliveryIdentifiers -- ^ 
  , deliveryOperationsName :: Text -- ^ The name of the metric you want to attribute to this \"delivery\".
  , deliveryOperationsAttributes :: DeliveryAttributes -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeliveryOperations where
  parseJSON = genericParseJSON optionsDeliveryOperations
instance ToJSON DeliveryOperations where
  toJSON = genericToJSON optionsDeliveryOperations

optionsDeliveryOperations :: Options
optionsDeliveryOperations =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("deliveryOperationsType", "type")
      , ("deliveryOperationsAction", "action")
      , ("deliveryOperationsIdentifiers", "identifiers")
      , ("deliveryOperationsName", "name")
      , ("deliveryOperationsAttributes", "attributes")
      ]


-- | The properties representing an individual device. [Our SDK&#39;s](/sdk/) gather all the properties defined below automatically, unless you disable the &#x60;autoTrackDeviceAttributes&#x60; setting. You can reference the properties outside the &#x60;attributes&#x60; object in segments or in Liquid.
data DeviceObject = DeviceObject
  { deviceObjectId :: Text -- ^ The device token.
  , deviceObjectLastUnderscoreused :: Maybe Int -- ^ The `timestamp` when you last identified this device. If you don't pass a timestamp when you add or update a device, we use the time of the request itself. Our SDKs identify a device when a person launches their app.
  , deviceObjectPlatform :: Text -- ^ The device/messaging platform.
  , deviceObjectAttributes :: Maybe AddDeviceRequestDeviceAllOfAttributes -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeviceObject where
  parseJSON = genericParseJSON optionsDeviceObject
instance ToJSON DeviceObject where
  toJSON = genericToJSON optionsDeviceObject

optionsDeviceObject :: Options
optionsDeviceObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("deviceObjectId", "id")
      , ("deviceObjectLastUnderscoreused", "last_used")
      , ("deviceObjectPlatform", "platform")
      , ("deviceObjectAttributes", "attributes")
      ]


-- | The properties representing an individual device in Journeys when sent from a Customer.io SDK that natively supports Data Pipelines (iOS 3.0, Android 4.0, or later versions). [Our Data Pipelines-enabled SDK&#39;s](/sdk/) gather all the properties defined below automatically unless you disable the &#x60;autoTrackDeviceAttributes&#x60; setting.
data DeviceObjectCdp = DeviceObjectCdp
  { deviceObjectCdpId :: Text -- ^ The device token.
  , deviceObjectCdpLastUnderscoreused :: Maybe Int -- ^ The `timestamp` when you last identified this device. If you don't pass a timestamp when you add or update a device, we use the time of the request itself. Our SDKs identify a device when a person launches their app.
  , deviceObjectCdpPlatform :: Text -- ^ The device/messaging platform.
  , deviceObjectCdpAttributes :: Maybe DeviceObjectCdpCommonAttributes -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeviceObjectCdp where
  parseJSON = genericParseJSON optionsDeviceObjectCdp
instance ToJSON DeviceObjectCdp where
  toJSON = genericToJSON optionsDeviceObjectCdp

optionsDeviceObjectCdp :: Options
optionsDeviceObjectCdp =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("deviceObjectCdpId", "id")
      , ("deviceObjectCdpLastUnderscoreused", "last_used")
      , ("deviceObjectCdpPlatform", "platform")
      , ("deviceObjectCdpAttributes", "attributes")
      ]


-- | Device information common to the v1 and v2 APIs.
data DeviceObjectCdpCommon = DeviceObjectCdpCommon
  { deviceObjectCdpCommonLastUnderscoreused :: Maybe Int -- ^ The `timestamp` when you last identified this device. If you don't pass a timestamp when you add or update a device, we use the time of the request itself. Our SDKs identify a device when a person launches their app.
  , deviceObjectCdpCommonPlatform :: Text -- ^ The device/messaging platform.
  , deviceObjectCdpCommonAttributes :: Maybe DeviceObjectCdpCommonAttributes -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeviceObjectCdpCommon where
  parseJSON = genericParseJSON optionsDeviceObjectCdpCommon
instance ToJSON DeviceObjectCdpCommon where
  toJSON = genericToJSON optionsDeviceObjectCdpCommon

optionsDeviceObjectCdpCommon :: Options
optionsDeviceObjectCdpCommon =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("deviceObjectCdpCommonLastUnderscoreused", "last_used")
      , ("deviceObjectCdpCommonPlatform", "platform")
      , ("deviceObjectCdpCommonAttributes", "attributes")
      ]


-- | Attributes that you can reference to segment your audience—like a person&#39;s attributes, but specific to a device. These can be either the attributes defined below or custom key-value attributes.
newtype DeviceObjectCdpCommonAttributes = DeviceObjectCdpCommonAttributes { unDeviceObjectCdpCommonAttributes :: (Map.Map Text Text) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | Device information common to the v1 and v2 APIs.
data DeviceObjectCommon = DeviceObjectCommon
  { deviceObjectCommonLastUnderscoreused :: Maybe Int -- ^ The `timestamp` when you last identified this device. If you don't pass a timestamp when you add or update a device, we use the time of the request itself. Our SDKs identify a device when a person launches their app.
  , deviceObjectCommonPlatform :: Text -- ^ The device/messaging platform.
  , deviceObjectCommonAttributes :: Maybe AddDeviceRequestDeviceAllOfAttributes -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeviceObjectCommon where
  parseJSON = genericParseJSON optionsDeviceObjectCommon
instance ToJSON DeviceObjectCommon where
  toJSON = genericToJSON optionsDeviceObjectCommon

optionsDeviceObjectCommon :: Options
optionsDeviceObjectCommon =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("deviceObjectCommonLastUnderscoreused", "last_used")
      , ("deviceObjectCommonPlatform", "platform")
      , ("deviceObjectCommonAttributes", "attributes")
      ]


-- | 
data Email = Email
  { emailDeliveryUnderscoreid :: Text -- ^ The CIO-Delivery-ID from the notification that you want to associate the `event` with.
  , emailTimestamp :: Maybe Int -- ^ The unix timestamp when the event occurred.
  , emailMetric :: Text -- ^ The email metric you want to report back to Customer.io.
  , emailRecipient :: Maybe Text -- ^ The email of the person who received the message.
  , emailReason :: Maybe Text -- ^ For metrics indicating a failure (like `bounced`), this field provides the reason for the failure.
  , emailHref :: Maybe Text -- ^ For `clicked` metrics, this is the link the recipient clicked.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Email where
  parseJSON = genericParseJSON optionsEmail
instance ToJSON Email where
  toJSON = genericToJSON optionsEmail

optionsEmail :: Options
optionsEmail =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("emailDeliveryUnderscoreid", "delivery_id")
      , ("emailTimestamp", "timestamp")
      , ("emailMetric", "metric")
      , ("emailRecipient", "recipient")
      , ("emailReason", "reason")
      , ("emailHref", "href")
      ]


-- | 
data Email1 = Email1
  { email1Email :: Text -- ^ The email address of the customer.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Email1 where
  parseJSON = genericParseJSON optionsEmail1
instance ToJSON Email1 where
  toJSON = genericToJSON optionsEmail1

optionsEmail1 :: Options
optionsEmail1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("email1Email", "email")
      ]


-- | 
data Email2 = Email2
  { email2Email :: Maybe Text -- ^ The email address of the customer.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Email2 where
  parseJSON = genericParseJSON optionsEmail2
instance ToJSON Email2 where
  toJSON = genericToJSON optionsEmail2

optionsEmail2 :: Options
optionsEmail2 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("email2Email", "email")
      ]


-- | 
data Email3 = Email3
  { email3Email :: Text -- ^ The identifier for the person represented by the transactional message. Use this option if your workspace identifies people by email rather than by `id`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Email3 where
  parseJSON = genericParseJSON optionsEmail3
instance ToJSON Email3 where
  toJSON = genericToJSON optionsEmail3

optionsEmail3 :: Options
optionsEmail3 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("email3Email", "email")
      ]


-- | Describes the email events reported from Customer.io to a webhook.
data EmailEvents = EmailEvents
  { emailEventsEmailUnderscoreattempted :: Maybe Bool -- ^ Reports when a message cannot be sent to the delivery provider and will retry. Set to true to report this event type.
  , emailEventsEmailUnderscorebounced :: Maybe Bool -- ^ Reports when the delivery provider is unable to deliver a message. Set to true to report this event type.
  , emailEventsEmailUnderscoreclicked :: Maybe Bool -- ^ Reports when a person clicks a tracked link in a message. Set to true to report this event type.
  , emailEventsEmailUnderscoreconverted :: Maybe Bool -- ^ Reports a conversion. Set to true to report this event type.
  , emailEventsEmailUnderscoredeferred :: Maybe Bool -- ^ Reports when the delivery provider couldn't send a message and will retry. Set to true to report this event type.
  , emailEventsEmailUnderscoredelivered :: Maybe Bool -- ^ Reports when the delivery provider reports that a message is delivered to an inbox. Set to true to report this event type.
  , emailEventsEmailUnderscoredrafted :: Maybe Bool -- ^ Reports when a message draft is created. Set to true to report this event type.
  , emailEventsEmailUnderscoredropped :: Maybe Bool -- ^ Reports when a message isn't sent because the recipient is suppressed. Set to true to report this event type.
  , emailEventsEmailUnderscorefailed :: Maybe Bool -- ^ Reports when an email couldn't be sent to the delivery provider. Set to true to report this event type.
  , emailEventsEmailUnderscoreopened :: Maybe Bool -- ^ Reports when a recipient opens a message. Set to true to report this event type.
  , emailEventsEmailUnderscoresent :: Maybe Bool -- ^ Reports when a message is sent from Customer.io to the delivery provider. Set to true to report this event type.
  , emailEventsEmailUnderscorespammed :: Maybe Bool -- ^ Reports a recipient marks a message as spam. Set to true to report this even type.
  , emailEventsEmailUnderscoreunsubscribed :: Maybe Bool -- ^ Reports when a person unsubscribes through a particular email. Set to true to report this event type.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailEvents where
  parseJSON = genericParseJSON optionsEmailEvents
instance ToJSON EmailEvents where
  toJSON = genericToJSON optionsEmailEvents

optionsEmailEvents :: Options
optionsEmailEvents =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("emailEventsEmailUnderscoreattempted", "email_attempted")
      , ("emailEventsEmailUnderscorebounced", "email_bounced")
      , ("emailEventsEmailUnderscoreclicked", "email_clicked")
      , ("emailEventsEmailUnderscoreconverted", "email_converted")
      , ("emailEventsEmailUnderscoredeferred", "email_deferred")
      , ("emailEventsEmailUnderscoredelivered", "email_delivered")
      , ("emailEventsEmailUnderscoredrafted", "email_drafted")
      , ("emailEventsEmailUnderscoredropped", "email_dropped")
      , ("emailEventsEmailUnderscorefailed", "email_failed")
      , ("emailEventsEmailUnderscoreopened", "email_opened")
      , ("emailEventsEmailUnderscoresent", "email_sent")
      , ("emailEventsEmailUnderscorespammed", "email_spammed")
      , ("emailEventsEmailUnderscoreunsubscribed", "email_unsubscribed")
      ]


-- | 
data EmailMessage = EmailMessage
  { emailMessageId :: Maybe Int -- ^ The identifier for an action.
  , emailMessageBroadcastUnderscoreid :: Maybe Int -- ^ The identifier for a broadcast.
  , emailMessageDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , emailMessageName :: Maybe Text -- ^ The name of the action, if it exists.
  , emailMessageLayout :: Maybe Text -- ^ The layout used for the action, if it exists.
  , emailMessageCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , emailMessageUpdated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , emailMessageBody :: Maybe Text -- ^ The body of the action. You cannot modify the body if you created it with our drag-and-drop editor.
  , emailMessageType :: Maybe Text -- ^ The type of action.
  , emailMessageSendingUnderscorestate :: Maybe Text -- ^ Determines the sending behavior for the action. `automatic` sends the action automatically when triggered; `draft` queues drafts when the action is triggered; or `off` to disable the action.
  , emailMessageLanguage :: Maybe Text -- ^ The language variant for your message. If you don't use our [localization feature](/localization), or this is the default message, this value is an empty string.
  , emailMessageFrom :: Maybe Text -- ^ The address that the message is from, relevant if the action `type` is `email`.
  , emailMessageFromUnderscoreid :: Maybe Int -- ^ The identifier of the `from` address, commonly known as the \"sender\". You can [list your sender identities](#operation/listSenders) to match the ID to a specific address.
  , emailMessageReplyUnderscoreto :: Maybe Text -- ^ The address that receives replies for the message, if applicable.
  , emailMessageReplyUnderscoretoUnderscoreid :: Maybe Int -- ^ The identifier for the `reply_to` address, if applicable. You can [list your sender identities](#operation/listSenders) to match the ID to a specific address.
  , emailMessagePreprocessor :: Maybe Text -- ^ By default, we process CSS before emails leave Customer.io using Premailer. If your message included CSS and pre-processing is not disabled, this key indicates the pre-processor.
  , emailMessageRecipient :: Maybe Text -- ^ The recipient address for an action.
  , emailMessageSubject :: Maybe Text -- ^ The subject line for an `email` action.
  , emailMessageBcc :: Maybe Text -- ^ The blind-copy address(es) for this action.
  , emailMessageFakeUnderscorebcc :: Maybe Bool -- ^ If true, rather than sending true copies to BCC addresses, Customer.io sends a copy of the message with the subject line containing the recipient address(es). 
  , emailMessagePreheaderUnderscoretext :: Maybe Text -- ^ Also known as \"preview text\", this specifies the small block of text shown in an end-user's email inbox, next to, or underneath, the subject line.
  , emailMessageHeaders :: Maybe (Map.Map String Text) -- ^ An object containing headers, where the key is the header name and the value is the header value. Header names and values must be strings and cannot contain any non-ASCII characters or empty spaces. Some headers are reserved and cannot be overwritten.
  , emailMessageBodyUnderscoreamp :: Maybe Text -- ^ If your message is an email, this is the AMP-enabled body of your message. If your recipient's email client doesn't support AMP, the `body` represents your fallback message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailMessage where
  parseJSON = genericParseJSON optionsEmailMessage
instance ToJSON EmailMessage where
  toJSON = genericToJSON optionsEmailMessage

optionsEmailMessage :: Options
optionsEmailMessage =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("emailMessageId", "id")
      , ("emailMessageBroadcastUnderscoreid", "broadcast_id")
      , ("emailMessageDeduplicateUnderscoreid", "deduplicate_id")
      , ("emailMessageName", "name")
      , ("emailMessageLayout", "layout")
      , ("emailMessageCreated", "created")
      , ("emailMessageUpdated", "updated")
      , ("emailMessageBody", "body")
      , ("emailMessageType", "type")
      , ("emailMessageSendingUnderscorestate", "sending_state")
      , ("emailMessageLanguage", "language")
      , ("emailMessageFrom", "from")
      , ("emailMessageFromUnderscoreid", "from_id")
      , ("emailMessageReplyUnderscoreto", "reply_to")
      , ("emailMessageReplyUnderscoretoUnderscoreid", "reply_to_id")
      , ("emailMessagePreprocessor", "preprocessor")
      , ("emailMessageRecipient", "recipient")
      , ("emailMessageSubject", "subject")
      , ("emailMessageBcc", "bcc")
      , ("emailMessageFakeUnderscorebcc", "fake_bcc")
      , ("emailMessagePreheaderUnderscoretext", "preheader_text")
      , ("emailMessageHeaders", "headers")
      , ("emailMessageBodyUnderscoreamp", "body_amp")
      ]


-- | 
data EmailMessage1 = EmailMessage1
  { emailMessage1Id :: Maybe Int -- ^ The identifier for an action.
  , emailMessage1CampaignUnderscoreid :: Maybe Int -- ^ The identifier for a campaign.
  , emailMessage1ParentUnderscoreactionUnderscoreid :: Maybe Int -- ^ The ID of the parent action, if the action occurred within a campaign and has a parent (like a randomized split, etc).
  , emailMessage1DeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , emailMessage1Name :: Maybe Text -- ^ The name of the action, if it exists.
  , emailMessage1Layout :: Maybe Text -- ^ The layout used for the action, if it exists.
  , emailMessage1Created :: Maybe Int -- ^ The date time when the referenced ID was created.
  , emailMessage1Updated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , emailMessage1Body :: Maybe Text -- ^ The body of the action. For emails, this is the HTML-body of a message. You cannot modify the body if you created it with our drag-and-drop editor.
  , emailMessage1BodyUnderscoreamp :: Maybe Text -- ^ If your message is an email, this is the AMP-enabled body of your message. If your recipient's email client doesn't support AMP, the `body` represents your fallback message.
  , emailMessage1Language :: Maybe Text -- ^ The language variant for your message. If you don't use our [localization feature](/localization), or this is the default message, this value is an empty string.
  , emailMessage1Type :: Maybe Text -- ^ The type of action.
  , emailMessage1SendingUnderscorestate :: Maybe Text -- ^ Determines the sending behavior for the action. `automatic` sends the action automatically when triggered; `draft` queues drafts when the action is triggered; or `off` to disable the action.
  , emailMessage1From :: Maybe Text -- ^ The address that the message is from, relevant if the action `type` is `email`.
  , emailMessage1FromUnderscoreid :: Maybe Int -- ^ The identifier of the `from` address, commonly known as the \"sender\". You can [list your sender identities](#operation/listSenders) to match the ID to a specific address.
  , emailMessage1ReplyUnderscoreto :: Maybe Text -- ^ The address that receives replies for the message, if applicable.
  , emailMessage1ReplyUnderscoretoUnderscoreid :: Maybe Int -- ^ The identifier for the `reply_to` address, if applicable. You can [list your sender identities](#operation/listSenders) to match the ID to a specific address.
  , emailMessage1Preprocessor :: Maybe Text -- ^ By default, we process CSS before emails leave Customer.io using Premailer. If your message included CSS and pre-processing is not disabled, this key indicates the pre-processor.
  , emailMessage1Recipient :: Maybe Text -- ^ The recipient address for an action.
  , emailMessage1Subject :: Maybe Text -- ^ The subject line for an `email` action.
  , emailMessage1Bcc :: Maybe Text -- ^ The blind-copy address(es) for this action.
  , emailMessage1FakeUnderscorebcc :: Maybe Bool -- ^ If true, rather than sending true copies to BCC addresses, Customer.io sends a copy of the message with the subject line containing the recipient address(es). 
  , emailMessage1PreheaderUnderscoretext :: Maybe Text -- ^ Also known as \"preview text\", this specifies the small block of text shown in an end-user's email inbox, next to, or underneath, the subject line.
  , emailMessage1Headers :: Maybe (Map.Map String Text) -- ^ An object containing headers, where the key is the header name and the value is the header value. Header names and values must be strings and cannot contain any non-ASCII characters or empty spaces. Some headers are reserved and cannot be overwritten.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailMessage1 where
  parseJSON = genericParseJSON optionsEmailMessage1
instance ToJSON EmailMessage1 where
  toJSON = genericToJSON optionsEmailMessage1

optionsEmailMessage1 :: Options
optionsEmailMessage1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("emailMessage1Id", "id")
      , ("emailMessage1CampaignUnderscoreid", "campaign_id")
      , ("emailMessage1ParentUnderscoreactionUnderscoreid", "parent_action_id")
      , ("emailMessage1DeduplicateUnderscoreid", "deduplicate_id")
      , ("emailMessage1Name", "name")
      , ("emailMessage1Layout", "layout")
      , ("emailMessage1Created", "created")
      , ("emailMessage1Updated", "updated")
      , ("emailMessage1Body", "body")
      , ("emailMessage1BodyUnderscoreamp", "body_amp")
      , ("emailMessage1Language", "language")
      , ("emailMessage1Type", "type")
      , ("emailMessage1SendingUnderscorestate", "sending_state")
      , ("emailMessage1From", "from")
      , ("emailMessage1FromUnderscoreid", "from_id")
      , ("emailMessage1ReplyUnderscoreto", "reply_to")
      , ("emailMessage1ReplyUnderscoretoUnderscoreid", "reply_to_id")
      , ("emailMessage1Preprocessor", "preprocessor")
      , ("emailMessage1Recipient", "recipient")
      , ("emailMessage1Subject", "subject")
      , ("emailMessage1Bcc", "bcc")
      , ("emailMessage1FakeUnderscorebcc", "fake_bcc")
      , ("emailMessage1PreheaderUnderscoretext", "preheader_text")
      , ("emailMessage1Headers", "headers")
      ]


-- | 
data Emails = Emails
  { emailsEmail :: Text -- ^ The email address of the recipient. This address must be unique in your workspace. If more than one person has the same `email` attribute, your request will produce an error.
  , emailsData :: Maybe (Map.Map String Value) -- ^ Merge data associated with the recipient.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Emails where
  parseJSON = genericParseJSON optionsEmails
instance ToJSON Emails where
  toJSON = genericToJSON optionsEmails

optionsEmails :: Options
optionsEmails =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("emailsEmail", "email")
      , ("emailsData", "data")
      ]


-- | 
data Entity400Response = Entity400Response
  { entity400ResponseErrors :: Maybe [Entity400ResponseErrorsInner] -- ^ An array of errors, where each object represents a different error.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Entity400Response where
  parseJSON = genericParseJSON optionsEntity400Response
instance ToJSON Entity400Response where
  toJSON = genericToJSON optionsEntity400Response

optionsEntity400Response :: Options
optionsEntity400Response =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("entity400ResponseErrors", "errors")
      ]


-- | 
data Entity400ResponseErrorsInner = Entity400ResponseErrorsInner
  { entity400ResponseErrorsInnerReason :: Maybe Text -- ^ The reason for the error.
  , entity400ResponseErrorsInnerField :: Maybe Text -- ^ The field containing the error.
  , entity400ResponseErrorsInnerMessage :: Maybe Text -- ^ A detailed description of the error in the offending field.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Entity400ResponseErrorsInner where
  parseJSON = genericParseJSON optionsEntity400ResponseErrorsInner
instance ToJSON Entity400ResponseErrorsInner where
  toJSON = genericToJSON optionsEntity400ResponseErrorsInner

optionsEntity400ResponseErrorsInner :: Options
optionsEntity400ResponseErrorsInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("entity400ResponseErrorsInnerReason", "reason")
      , ("entity400ResponseErrorsInnerField", "field")
      , ("entity400ResponseErrorsInnerMessage", "message")
      ]


-- | 
data EntityRequest = EntityRequest
  { entityRequestType :: Text -- ^ The operation modifies a person in Customer.io
  , entityRequestIdentifiers :: DeliveryIdentifiers -- ^ 
  , entityRequestAction :: Text -- ^ An `event` action indicates a delivery event. Use the `name` to determine the specific metric that you want to attribute to this delivery.
  , entityRequestAttributes :: DeliveryAttributes -- ^ 
  , entityRequestCioUnderscorerelationships :: [Identify1AllOfCioRelationshipsInner] -- ^ The people you want to associate with an object. Each object in the array represents a person.
  , entityRequestId :: Maybe Text -- ^ A valid ULID used to deduplicate events. Note - our Python and Ruby libraries do not pass this id.
  , entityRequestName :: Text -- ^ The name of the metric you want to attribute to this \"delivery\".
  , entityRequestTimestamp :: Maybe Int -- ^ The Unix timestamp when the event happened.
  , entityRequestDevice :: DeleteDeviceAllOfDevice -- ^ 
  , entityRequestPrimary :: PersonOneOf3Primary -- ^ 
  , entityRequestSecondary :: PersonOneOf3Secondary -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EntityRequest where
  parseJSON = genericParseJSON optionsEntityRequest
instance ToJSON EntityRequest where
  toJSON = genericToJSON optionsEntityRequest

optionsEntityRequest :: Options
optionsEntityRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("entityRequestType", "type")
      , ("entityRequestIdentifiers", "identifiers")
      , ("entityRequestAction", "action")
      , ("entityRequestAttributes", "attributes")
      , ("entityRequestCioUnderscorerelationships", "cio_relationships")
      , ("entityRequestId", "id")
      , ("entityRequestName", "name")
      , ("entityRequestTimestamp", "timestamp")
      , ("entityRequestDevice", "device")
      , ("entityRequestPrimary", "primary")
      , ("entityRequestSecondary", "secondary")
      ]


-- | 
data EspSuppression = EspSuppression
  { espSuppressionCategory :: Maybe Text -- ^ The reason the addresses are suppressed.
  , espSuppressionSuppressions :: Maybe [EspSuppressionSuppressionsInner] -- ^ The addresses suppressed in this category.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EspSuppression where
  parseJSON = genericParseJSON optionsEspSuppression
instance ToJSON EspSuppression where
  toJSON = genericToJSON optionsEspSuppression

optionsEspSuppression :: Options
optionsEspSuppression =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("espSuppressionCategory", "category")
      , ("espSuppressionSuppressions", "suppressions")
      ]


-- | 
data EspSuppressionSuppressionsInner = EspSuppressionSuppressionsInner
  { espSuppressionSuppressionsInnerCreated :: Maybe Int -- ^ The timestamp (in seconds), when the ESP suppressed the address.
  , espSuppressionSuppressionsInnerEmail :: Maybe Text -- ^ The email address that the ESP suppressed.
  , espSuppressionSuppressionsInnerReason :: Maybe Text -- ^ The reason for the suppression, as [recorded by Mailgun](https://documentation.mailgun.com/en/latest/api-suppressions.html).
  , espSuppressionSuppressionsInnerStatus :: Maybe Text -- ^ The status code for the suppression, as [recorded by mailgun](https://documentation.mailgun.com/en/latest/api-suppressions.html). This is normally `550`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EspSuppressionSuppressionsInner where
  parseJSON = genericParseJSON optionsEspSuppressionSuppressionsInner
instance ToJSON EspSuppressionSuppressionsInner where
  toJSON = genericToJSON optionsEspSuppressionSuppressionsInner

optionsEspSuppressionSuppressionsInner :: Options
optionsEspSuppressionSuppressionsInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("espSuppressionSuppressionsInnerCreated", "created")
      , ("espSuppressionSuppressionsInnerEmail", "email")
      , ("espSuppressionSuppressionsInnerReason", "reason")
      , ("espSuppressionSuppressionsInnerStatus", "status")
      ]


-- | 
data Event = Event
  { eventId :: Maybe Int -- ^ The identifier for a campaign.
  , eventDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , eventName :: Maybe Text -- ^ The name of the campaign.
  , eventType :: Maybe Text -- ^ The type of campaign trigger. **Sunsetting on March 30, 2025**
  , eventCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , eventUpdated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , eventActive :: Maybe Bool -- ^ If true, the campaign is active and can still send messages.
  , eventState :: Maybe Text -- ^ The status of the campaign.
  , eventActions :: Maybe [SegmentActionsInner] -- ^ An array of actions contained within the campaign.
  , eventFirstUnderscorestarted :: Maybe Int -- ^ The date and time when you first started the campaign and it first became eligible to be triggered.
  , eventTags :: Maybe [Text] -- ^ An array of tags you set on this campaign.
  , eventFilterUnderscoresegmentUnderscoreids :: Maybe [Int] -- ^ A list of segments used in the campaign filter, returned if the campaign audience was filtered on one or more segments.
  , eventEventUnderscorename :: Maybe Text -- ^ The name of the event. How you reference the event in campaigns or segments.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Event where
  parseJSON = genericParseJSON optionsEvent
instance ToJSON Event where
  toJSON = genericToJSON optionsEvent

optionsEvent :: Options
optionsEvent =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("eventId", "id")
      , ("eventDeduplicateUnderscoreid", "deduplicate_id")
      , ("eventName", "name")
      , ("eventType", "type")
      , ("eventCreated", "created")
      , ("eventUpdated", "updated")
      , ("eventActive", "active")
      , ("eventState", "state")
      , ("eventActions", "actions")
      , ("eventFirstUnderscorestarted", "first_started")
      , ("eventTags", "tags")
      , ("eventFilterUnderscoresegmentUnderscoreids", "filter_segment_ids")
      , ("eventEventUnderscorename", "event_name")
      ]


-- | 
data EventsRequest = EventsRequest
  { eventsRequestName :: Text -- ^ The name of the event. In general, this should be the name of the screen or deep link path that a person viewed, making it easy to segment your audience or trigger campaigns from these events. Make sure you trim leading and trailing spaces from this field.
  , eventsRequestId :: Maybe Text -- ^ An identifier used to deduplicate events. This value must be a [ULID](https://github.com/ulid/spec). If an event has the same value as an event we previously received, we won't show or process the duplicate. Note - our Python and Ruby libraries do not pass this id.
  , eventsRequestType :: Text -- ^ Sets the event type. If your event isn't a `page` or `screen` type event, we automatically set this property to `event`.
  , eventsRequestTimestamp :: Maybe Int -- ^ The unix timestamp when the event took place. If you don't provide this value, we use the date-time when we receive the event. 
  , eventsRequestData :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on your customer (referenced by `customer_id`).
  , eventsRequestAnonymousUnderscoreid :: Text -- ^ An identifier for an anonymous event, like a cookie. If set as an attribute on a person, any events bearing the same anonymous value are associated with this person. This value must be unique and is not reusable.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EventsRequest where
  parseJSON = genericParseJSON optionsEventsRequest
instance ToJSON EventsRequest where
  toJSON = genericToJSON optionsEventsRequest

optionsEventsRequest :: Options
optionsEventsRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("eventsRequestName", "name")
      , ("eventsRequestId", "id")
      , ("eventsRequestType", "type")
      , ("eventsRequestTimestamp", "timestamp")
      , ("eventsRequestData", "data")
      , ("eventsRequestAnonymousUnderscoreid", "anonymous_id")
      ]


-- | Metadata about an export.
data ExportObject = ExportObject
  { exportObjectId :: Maybe Int -- ^ The identifier for the export.
  , exportObjectUserUnderscoreid :: Maybe Int -- ^ The user who created the export.
  , exportObjectUserUnderscoreemail :: Maybe Text -- ^ The email of the user who created the export.
  , exportObjectTotal :: Maybe Int -- ^ The number of entries in the export. Exports report 0 until done.
  , exportObjectDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , exportObjectType :: Maybe Text -- ^ The type of information contained in the export.
  , exportObjectFailed :: Maybe Bool -- ^ If true, the export was unsuccessful.
  , exportObjectDescription :: Maybe Text -- ^ A description of the export.
  , exportObjectDownloads :: Maybe Int -- ^ Counts the total number of times the export has been downloaded.
  , exportObjectCreatedUnderscoreat :: Maybe Int -- ^ The date time when the referenced ID was created.
  , exportObjectUpdatedUnderscoreat :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , exportObjectStatus :: Maybe Text -- ^ The state of your export where `done` indicates an export that you can download, `pending`, indicates that your export is not ready to download, and `failed` indicates an export that has failed and will not be downloadable.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportObject where
  parseJSON = genericParseJSON optionsExportObject
instance ToJSON ExportObject where
  toJSON = genericToJSON optionsExportObject

optionsExportObject :: Options
optionsExportObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("exportObjectId", "id")
      , ("exportObjectUserUnderscoreid", "user_id")
      , ("exportObjectUserUnderscoreemail", "user_email")
      , ("exportObjectTotal", "total")
      , ("exportObjectDeduplicateUnderscoreid", "deduplicate_id")
      , ("exportObjectType", "type")
      , ("exportObjectFailed", "failed")
      , ("exportObjectDescription", "description")
      , ("exportObjectDownloads", "downloads")
      , ("exportObjectCreatedUnderscoreat", "created_at")
      , ("exportObjectUpdatedUnderscoreat", "updated_at")
      , ("exportObjectStatus", "status")
      ]


-- | Contains properties shared by export filters.
data ExportSharedProps = ExportSharedProps
  { exportSharedPropsStart :: Maybe Int -- ^ The unix timestamp representing the beginning of the export.
  , exportSharedPropsEnd :: Maybe Int -- ^ The unix timestamp representing the end of the export.
  , exportSharedPropsAttributes :: Maybe [Text] -- ^ The names of attributes you want to include in your export; each attribute name is an additional column in the export. If your message included liquid, you may add the attribute names used in your message so you can see the values populated for each delivery.
  , exportSharedPropsMetric :: Maybe Text -- ^ Determines the metric(s) you want to return.
  , exportSharedPropsDrafts :: Maybe Bool -- ^ If true, your request returns both drafts and active/sent messages.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportSharedProps where
  parseJSON = genericParseJSON optionsExportSharedProps
instance ToJSON ExportSharedProps where
  toJSON = genericToJSON optionsExportSharedProps

optionsExportSharedProps :: Options
optionsExportSharedProps =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("exportSharedPropsStart", "start")
      , ("exportSharedPropsEnd", "end")
      , ("exportSharedPropsAttributes", "attributes")
      , ("exportSharedPropsMetric", "metric")
      , ("exportSharedPropsDrafts", "drafts")
      ]


-- | The type of information contained in the export.
data ExportType = ExportType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportType where
  parseJSON = genericParseJSON optionsExportType
instance ToJSON ExportType where
  toJSON = genericToJSON optionsExportType

optionsExportType :: Options
optionsExportType =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data FCM = FCM
  { fCMMessage :: FCMMessage -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FCM where
  parseJSON = genericParseJSON optionsFCM
instance ToJSON FCM where
  toJSON = genericToJSON optionsFCM

optionsFCM :: Options
optionsFCM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fCMMessage", "message")
      ]


-- | The base object for all FCM payloads.
data FCMMessage = FCMMessage
  { fCMMessageApns :: FCMMessageApns -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FCMMessage where
  parseJSON = genericParseJSON optionsFCMMessage
instance ToJSON FCMMessage where
  toJSON = genericToJSON optionsFCMMessage

optionsFCMMessage :: Options
optionsFCMMessage =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fCMMessageApns", "apns")
      ]


-- | Defines a payload for iOS devices sent through Firebase Cloud Messaging (FCM).
data FCMMessageApns = FCMMessageApns
  { fCMMessageApnsHeaders :: Maybe Value -- ^ Headers defined by [Apple's payload reference](https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/sending_notification_requests_to_apns) that you want to pass through FCM.
  , fCMMessageApnsPayload :: FCMMessageApnsPayload -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FCMMessageApns where
  parseJSON = genericParseJSON optionsFCMMessageApns
instance ToJSON FCMMessageApns where
  toJSON = genericToJSON optionsFCMMessageApns

optionsFCMMessageApns :: Options
optionsFCMMessageApns =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fCMMessageApnsHeaders", "headers")
      , ("fCMMessageApnsPayload", "payload")
      ]


-- | Contains a push payload.
newtype FCMMessageApnsPayload = FCMMessageApnsPayload { unFCMMessageApnsPayload :: (Map.Map Text Value) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | A push payload intended for an iOS device.
data FCMMessageApnsPayloadAps = FCMMessageApnsPayloadAps
  { fCMMessageApnsPayloadApsAlert :: Maybe FCMMessageApnsPayloadApsAlert -- ^ 
  , fCMMessageApnsPayloadApsBadge :: Maybe Int -- ^ The number you want to display on your app's icon. Set to 0 to remove the current badge, if any.
  , fCMMessageApnsPayloadApsSound :: Maybe FCMMessageApnsPayloadApsSound -- ^ 
  , fCMMessageApnsPayloadApsThreadDashid :: Maybe Text -- ^ An identifier to group related notifications.
  , fCMMessageApnsPayloadApsCategory :: Maybe Text -- ^ The notification’s type. This string must correspond to the identifier of one of the `UNNotificationCategory` objects you register at launch time.
  , fCMMessageApnsPayloadApsContentDashavailable :: Maybe Int -- ^ The background notification flag. Use `1` without an `alert` to perform a silent update. `0` indicates a normal push notification.
  , fCMMessageApnsPayloadApsMutableDashcontent :: Maybe Int -- ^ If you use the Customer.io SDK, you *must* set this value to `1` to support images and \"delivered\" metrics from your push notifications. When the value is 1, your notification is passed to your notification service app extension before delivery. Use your extension to modify the notification’s content. 
  , fCMMessageApnsPayloadApsTargetDashcontentDashid :: Maybe Text -- ^ The identifier of the window brought forward.
  , fCMMessageApnsPayloadApsInterruptionDashlevel :: Maybe Text -- ^ Indicates the importance and delivery timing of a notification.
  , fCMMessageApnsPayloadApsRelevanceDashscore :: Maybe Double -- ^ A number between 0 and 1. The highest score is considered the \"most relevant\"  and is featured in the notification summary.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FCMMessageApnsPayloadAps where
  parseJSON = genericParseJSON optionsFCMMessageApnsPayloadAps
instance ToJSON FCMMessageApnsPayloadAps where
  toJSON = genericToJSON optionsFCMMessageApnsPayloadAps

optionsFCMMessageApnsPayloadAps :: Options
optionsFCMMessageApnsPayloadAps =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fCMMessageApnsPayloadApsAlert", "alert")
      , ("fCMMessageApnsPayloadApsBadge", "badge")
      , ("fCMMessageApnsPayloadApsSound", "sound")
      , ("fCMMessageApnsPayloadApsThreadDashid", "thread-id")
      , ("fCMMessageApnsPayloadApsCategory", "category")
      , ("fCMMessageApnsPayloadApsContentDashavailable", "content-available")
      , ("fCMMessageApnsPayloadApsMutableDashcontent", "mutable-content")
      , ("fCMMessageApnsPayloadApsTargetDashcontentDashid", "target-content-id")
      , ("fCMMessageApnsPayloadApsInterruptionDashlevel", "interruption-level")
      , ("fCMMessageApnsPayloadApsRelevanceDashscore", "relevance-score")
      ]


-- | 
data FCMMessageApnsPayloadApsAlert = FCMMessageApnsPayloadApsAlert
  { fCMMessageApnsPayloadApsAlertBody :: Maybe Text -- ^ The body of your push notification.
  , fCMMessageApnsPayloadApsAlertTitle :: Maybe Text -- ^ The title of your push notification.
  , fCMMessageApnsPayloadApsAlertSubtitle :: Maybe Text -- ^ Additional information that explains the purpose of the notification.
  , fCMMessageApnsPayloadApsAlertLaunchDashimage :: Maybe Text -- ^ The name of the launch image file you want to display. When a user launches your app, they'll see this image or storyboard file rather than your app’s normal launch image.
  , fCMMessageApnsPayloadApsAlertTitleDashlocDashkey :: Maybe Text -- ^ The key for a localized title string in your app’s Localizable.strings files.
  , fCMMessageApnsPayloadApsAlertTitleDashlocDashargs :: Maybe [Text] -- ^ An array of replacement value strings for variables in your title string. Each %@ character in the title-loc-key is replaced by a value from this array, in the order they appear in the title string.
  , fCMMessageApnsPayloadApsAlertSubtitleDashlocDashkey :: Maybe Text -- ^ The key for a localized subtitle string in your app’s Localizable.strings file.
  , fCMMessageApnsPayloadApsAlertSubtitleDashlocDashargs :: Maybe [Text] -- ^ An array of replacement value strings for variables in your subtitle string. Each %@ character in the subtitle-loc-key is replaced by a value from this array, in the order they appear in the subtitle string.
  , fCMMessageApnsPayloadApsAlertLocDashkey :: Maybe Text -- ^ The key for a localized message string in your app’s Localizable.strings file.
  , fCMMessageApnsPayloadApsAlertLocDashargs :: Maybe [Text] -- ^ An array of replacement value strings for variables in your message text. Each %@ character in the loc-key is replaced by a value from this array, in the order they appear in the message body.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FCMMessageApnsPayloadApsAlert where
  parseJSON = genericParseJSON optionsFCMMessageApnsPayloadApsAlert
instance ToJSON FCMMessageApnsPayloadApsAlert where
  toJSON = genericToJSON optionsFCMMessageApnsPayloadApsAlert

optionsFCMMessageApnsPayloadApsAlert :: Options
optionsFCMMessageApnsPayloadApsAlert =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fCMMessageApnsPayloadApsAlertBody", "body")
      , ("fCMMessageApnsPayloadApsAlertTitle", "title")
      , ("fCMMessageApnsPayloadApsAlertSubtitle", "subtitle")
      , ("fCMMessageApnsPayloadApsAlertLaunchDashimage", "launch-image")
      , ("fCMMessageApnsPayloadApsAlertTitleDashlocDashkey", "title-loc-key")
      , ("fCMMessageApnsPayloadApsAlertTitleDashlocDashargs", "title-loc-args")
      , ("fCMMessageApnsPayloadApsAlertSubtitleDashlocDashkey", "subtitle-loc-key")
      , ("fCMMessageApnsPayloadApsAlertSubtitleDashlocDashargs", "subtitle-loc-args")
      , ("fCMMessageApnsPayloadApsAlertLocDashkey", "loc-key")
      , ("fCMMessageApnsPayloadApsAlertLocDashargs", "loc-args")
      ]


-- | 
data FCMMessageApnsPayloadApsSound = FCMMessageApnsPayloadApsSound
  { fCMMessageApnsPayloadApsSoundCritical :: Maybe Int -- ^ 1 indicates critical. 0 is not critical.
  , fCMMessageApnsPayloadApsSoundName :: Maybe Text -- ^ The name of a sound file in your app’s main bundle or in the Library/Sounds folder of your app’s container directory. Use “default” to play the system sound.
  , fCMMessageApnsPayloadApsSoundVolume :: Maybe Double -- ^ The volume for a critical alert between 0 and 1, where 0 is silent and 1 is full volume.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FCMMessageApnsPayloadApsSound where
  parseJSON = genericParseJSON optionsFCMMessageApnsPayloadApsSound
instance ToJSON FCMMessageApnsPayloadApsSound where
  toJSON = genericToJSON optionsFCMMessageApnsPayloadApsSound

optionsFCMMessageApnsPayloadApsSound :: Options
optionsFCMMessageApnsPayloadApsSound =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fCMMessageApnsPayloadApsSoundCritical", "critical")
      , ("fCMMessageApnsPayloadApsSoundName", "name")
      , ("fCMMessageApnsPayloadApsSoundVolume", "volume")
      ]


-- | Contains properties interpreted by the Customer.io iOS SDK.
data FCMMessageApnsPayloadCIO = FCMMessageApnsPayloadCIO
  { fCMMessageApnsPayloadCIOPush :: FCMMessageApnsPayloadCIOPush -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FCMMessageApnsPayloadCIO where
  parseJSON = genericParseJSON optionsFCMMessageApnsPayloadCIO
instance ToJSON FCMMessageApnsPayloadCIO where
  toJSON = genericToJSON optionsFCMMessageApnsPayloadCIO

optionsFCMMessageApnsPayloadCIO :: Options
optionsFCMMessageApnsPayloadCIO =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fCMMessageApnsPayloadCIOPush", "push")
      ]


-- | A push payload for the iOS SDK.
data FCMMessageApnsPayloadCIOPush = FCMMessageApnsPayloadCIOPush
  { fCMMessageApnsPayloadCIOPushTitle :: Maybe Text -- ^ The title of your push notification.
  , fCMMessageApnsPayloadCIOPushBody :: Maybe Text -- ^ The body of your push notification.
  , fCMMessageApnsPayloadCIOPushLink :: Maybe Text -- ^ A deep link (to a page in your app), or a link to a web page.
  , fCMMessageApnsPayloadCIOPushImage :: Maybe Text -- ^ The URL of an HTTPS image that you want to use for your message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FCMMessageApnsPayloadCIOPush where
  parseJSON = genericParseJSON optionsFCMMessageApnsPayloadCIOPush
instance ToJSON FCMMessageApnsPayloadCIOPush where
  toJSON = genericToJSON optionsFCMMessageApnsPayloadCIOPush

optionsFCMMessageApnsPayloadCIOPush :: Options
optionsFCMMessageApnsPayloadCIOPush =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fCMMessageApnsPayloadCIOPushTitle", "title")
      , ("fCMMessageApnsPayloadCIOPushBody", "body")
      , ("fCMMessageApnsPayloadCIOPushLink", "link")
      , ("fCMMessageApnsPayloadCIOPushImage", "image")
      ]


-- | 
data FcmAndroid = FcmAndroid
  { fcmAndroidMessage :: TransactionalSharedPushObjectCustomPayloadAndroid -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FcmAndroid where
  parseJSON = genericParseJSON optionsFcmAndroid
instance ToJSON FcmAndroid where
  toJSON = genericToJSON optionsFcmAndroid

optionsFcmAndroid :: Options
optionsFcmAndroid =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fcmAndroidMessage", "message")
      ]


-- | 
data FcmAndroidWithSdk = FcmAndroidWithSdk
  { fcmAndroidWithSdkMessage :: FcmAndroidWithSdkMessage -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FcmAndroidWithSdk where
  parseJSON = genericParseJSON optionsFcmAndroidWithSdk
instance ToJSON FcmAndroidWithSdk where
  toJSON = genericToJSON optionsFcmAndroidWithSdk

optionsFcmAndroidWithSdk :: Options
optionsFcmAndroidWithSdk =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fcmAndroidWithSdkMessage", "message")
      ]


-- | The parent object for all push payloads.
data FcmAndroidWithSdkMessage = FcmAndroidWithSdkMessage
  { fcmAndroidWithSdkMessageData :: FcmBasicPushMessageData -- ^ 
  , fcmAndroidWithSdkMessageAndroid :: Maybe DataObjectOnlyAndroid -- ^ 
  , fcmAndroidWithSdkMessageNotification :: NotificationAndDataObjectNotification -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FcmAndroidWithSdkMessage where
  parseJSON = genericParseJSON optionsFcmAndroidWithSdkMessage
instance ToJSON FcmAndroidWithSdkMessage where
  toJSON = genericToJSON optionsFcmAndroidWithSdkMessage

optionsFcmAndroidWithSdkMessage :: Options
optionsFcmAndroidWithSdkMessage =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fcmAndroidWithSdkMessageData", "data")
      , ("fcmAndroidWithSdkMessageAndroid", "android")
      , ("fcmAndroidWithSdkMessageNotification", "notification")
      ]


-- | A custom push payload for Android devices.
data FcmAndroidWithoutSdk = FcmAndroidWithoutSdk
  { fcmAndroidWithoutSdkMessage :: TransactionalSharedPushObjectCustomPayloadAndroidMessage -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FcmAndroidWithoutSdk where
  parseJSON = genericParseJSON optionsFcmAndroidWithoutSdk
instance ToJSON FcmAndroidWithoutSdk where
  toJSON = genericToJSON optionsFcmAndroidWithoutSdk

optionsFcmAndroidWithoutSdk :: Options
optionsFcmAndroidWithoutSdk =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fcmAndroidWithoutSdkMessage", "message")
      ]


-- | 
data FcmBasicPush = FcmBasicPush
  { fcmBasicPushMessage :: FcmBasicPushMessage -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FcmBasicPush where
  parseJSON = genericParseJSON optionsFcmBasicPush
instance ToJSON FcmBasicPush where
  toJSON = genericToJSON optionsFcmBasicPush

optionsFcmBasicPush :: Options
optionsFcmBasicPush =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fcmBasicPushMessage", "message")
      ]


-- | The parent object for all custom push payloads.
data FcmBasicPushMessage = FcmBasicPushMessage
  { fcmBasicPushMessageNotification :: FcmBasicPushMessageNotification -- ^ 
  , fcmBasicPushMessageData :: Maybe FcmBasicPushMessageData -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FcmBasicPushMessage where
  parseJSON = genericParseJSON optionsFcmBasicPushMessage
instance ToJSON FcmBasicPushMessage where
  toJSON = genericToJSON optionsFcmBasicPushMessage

optionsFcmBasicPushMessage :: Options
optionsFcmBasicPushMessage =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fcmBasicPushMessageNotification", "notification")
      , ("fcmBasicPushMessageData", "data")
      ]


-- | Contains the &#x60;link&#x60; property (interpreted by the SDK) and additional properties that you want to pass to your app.
data FcmBasicPushMessageData = FcmBasicPushMessageData
  { fcmBasicPushMessageDataLink :: Maybe Text -- ^ A deep link (to a page in your app), or a link to a web page.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FcmBasicPushMessageData where
  parseJSON = genericParseJSON optionsFcmBasicPushMessageData
instance ToJSON FcmBasicPushMessageData where
  toJSON = genericToJSON optionsFcmBasicPushMessageData

optionsFcmBasicPushMessageData :: Options
optionsFcmBasicPushMessageData =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fcmBasicPushMessageDataLink", "link")
      ]


-- | Contains properties interpreted by the SDK except for the &#x60;link&#x60;.
data FcmBasicPushMessageNotification = FcmBasicPushMessageNotification
  { fcmBasicPushMessageNotificationTitle :: Maybe Text -- ^ The title of your push notification.
  , fcmBasicPushMessageNotificationBody :: Text -- ^ The body of your push notification.
  , fcmBasicPushMessageNotificationImage :: Maybe Text -- ^ The URL of an HTTPS image that you want to use for your message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FcmBasicPushMessageNotification where
  parseJSON = genericParseJSON optionsFcmBasicPushMessageNotification
instance ToJSON FcmBasicPushMessageNotification where
  toJSON = genericToJSON optionsFcmBasicPushMessageNotification

optionsFcmBasicPushMessageNotification :: Options
optionsFcmBasicPushMessageNotification =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fcmBasicPushMessageNotificationTitle", "title")
      , ("fcmBasicPushMessageNotificationBody", "body")
      , ("fcmBasicPushMessageNotificationImage", "image")
      ]


-- | 
data FcmIosWithSdk = FcmIosWithSdk
  { fcmIosWithSdkMessage :: FCMMessage -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FcmIosWithSdk where
  parseJSON = genericParseJSON optionsFcmIosWithSdk
instance ToJSON FcmIosWithSdk where
  toJSON = genericToJSON optionsFcmIosWithSdk

optionsFcmIosWithSdk :: Options
optionsFcmIosWithSdk =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fcmIosWithSdkMessage", "message")
      ]


-- | 
data FcmIosWithoutSdk = FcmIosWithoutSdk
  { fcmIosWithoutSdkMessage :: FcmIosWithoutSdkMessage -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FcmIosWithoutSdk where
  parseJSON = genericParseJSON optionsFcmIosWithoutSdk
instance ToJSON FcmIosWithoutSdk where
  toJSON = genericToJSON optionsFcmIosWithoutSdk

optionsFcmIosWithoutSdk :: Options
optionsFcmIosWithoutSdk =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fcmIosWithoutSdkMessage", "message")
      ]


-- | The base object for all Firebase payloads.
data FcmIosWithoutSdkMessage = FcmIosWithoutSdkMessage
  { fcmIosWithoutSdkMessageApns :: FcmIosWithoutSdkMessageApns -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FcmIosWithoutSdkMessage where
  parseJSON = genericParseJSON optionsFcmIosWithoutSdkMessage
instance ToJSON FcmIosWithoutSdkMessage where
  toJSON = genericToJSON optionsFcmIosWithoutSdkMessage

optionsFcmIosWithoutSdkMessage :: Options
optionsFcmIosWithoutSdkMessage =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fcmIosWithoutSdkMessageApns", "apns")
      ]


-- | Defines a push notification for iOS devices.
data FcmIosWithoutSdkMessageApns = FcmIosWithoutSdkMessageApns
  { fcmIosWithoutSdkMessageApnsHeaders :: Maybe Value -- ^ Headers defined by [Apple's payload reference](https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/sending_notification_requests_to_apns) that you want to pass through FCM.
  , fcmIosWithoutSdkMessageApnsPayload :: FcmIosWithoutSdkMessageApnsPayload -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FcmIosWithoutSdkMessageApns where
  parseJSON = genericParseJSON optionsFcmIosWithoutSdkMessageApns
instance ToJSON FcmIosWithoutSdkMessageApns where
  toJSON = genericToJSON optionsFcmIosWithoutSdkMessageApns

optionsFcmIosWithoutSdkMessageApns :: Options
optionsFcmIosWithoutSdkMessageApns =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fcmIosWithoutSdkMessageApnsHeaders", "headers")
      , ("fcmIosWithoutSdkMessageApnsPayload", "payload")
      ]


-- | 
data FcmIosWithoutSdkMessageApnsPayload = FcmIosWithoutSdkMessageApnsPayload
  { fcmIosWithoutSdkMessageApnsPayloadAps :: Maybe FCMMessageApnsPayloadAps -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FcmIosWithoutSdkMessageApnsPayload where
  parseJSON = genericParseJSON optionsFcmIosWithoutSdkMessageApnsPayload
instance ToJSON FcmIosWithoutSdkMessageApnsPayload where
  toJSON = genericToJSON optionsFcmIosWithoutSdkMessageApnsPayload

optionsFcmIosWithoutSdkMessageApnsPayload :: Options
optionsFcmIosWithoutSdkMessageApnsPayload =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fcmIosWithoutSdkMessageApnsPayloadAps", "aps")
      ]


-- | Used to layout an in-app message in a fixed grid.
data FixedGridWidget = FixedGridWidget
  { fixedGridWidgetType :: Text -- ^ Defines the widget type.
  , fixedGridWidgetItemPadding :: Maybe Text -- ^ The padding between items in your grid.
  , fixedGridWidgetColumns :: Int -- ^ The number of columns in your grid.
  , fixedGridWidgetChildAspectRatio :: Maybe Double -- ^ The aspect ratio for items in the grid. Defaults to 1.0
  , fixedGridWidgetComponents :: [Value] -- ^ An array of child components that you want to make available inside this widget.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FixedGridWidget where
  parseJSON = genericParseJSON optionsFixedGridWidget
instance ToJSON FixedGridWidget where
  toJSON = genericToJSON optionsFixedGridWidget

optionsFixedGridWidget :: Options
optionsFixedGridWidget =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fixedGridWidgetType", "type")
      , ("fixedGridWidgetItemPadding", "itemPadding")
      , ("fixedGridWidgetColumns", "columns")
      , ("fixedGridWidgetChildAspectRatio", "childAspectRatio")
      , ("fixedGridWidgetComponents", "components")
      ]


-- | Displays a series of components in a horizontal list, like if you want to put an image and text next to each other.
data FixedHorizontalListWidget = FixedHorizontalListWidget
  { fixedHorizontalListWidgetType :: Maybe Text -- ^ Defines the widget type.
  , fixedHorizontalListWidgetMainAxisAlignment :: Maybe Text -- ^ The mainAxisAlignment property supports the following options.
  , fixedHorizontalListWidgetCrossAxisAlignment :: Maybe Text -- ^ The crossAxisAlignment property supports the following options
  , fixedHorizontalListWidgetComponents :: Maybe [Value] -- ^ An array of child components that you want to make available inside this widget.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FixedHorizontalListWidget where
  parseJSON = genericParseJSON optionsFixedHorizontalListWidget
instance ToJSON FixedHorizontalListWidget where
  toJSON = genericToJSON optionsFixedHorizontalListWidget

optionsFixedHorizontalListWidget :: Options
optionsFixedHorizontalListWidget =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fixedHorizontalListWidgetType", "type")
      , ("fixedHorizontalListWidgetMainAxisAlignment", "mainAxisAlignment")
      , ("fixedHorizontalListWidgetCrossAxisAlignment", "crossAxisAlignment")
      , ("fixedHorizontalListWidgetComponents", "components")
      ]


-- | A series of components that you can scroll through, like a carousel.
data FixedHorizontalScrollWidget = FixedHorizontalScrollWidget
  { fixedHorizontalScrollWidgetType :: Maybe Text -- ^ Defines the widget type
  , fixedHorizontalScrollWidgetHeight :: Maybe Int -- ^ The height of the widget in pixels.
  , fixedHorizontalScrollWidgetComponents :: Maybe [Value] -- ^ An array of child components that you want to make available inside this widget.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FixedHorizontalScrollWidget where
  parseJSON = genericParseJSON optionsFixedHorizontalScrollWidget
instance ToJSON FixedHorizontalScrollWidget where
  toJSON = genericToJSON optionsFixedHorizontalScrollWidget

optionsFixedHorizontalScrollWidget :: Options
optionsFixedHorizontalScrollWidget =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fixedHorizontalScrollWidgetType", "type")
      , ("fixedHorizontalScrollWidgetHeight", "height")
      , ("fixedHorizontalScrollWidgetComponents", "components")
      ]


-- | Displays a series of components in a vertical list.
data FixedListWidget = FixedListWidget
  { fixedListWidgetType :: Maybe Text -- ^ Defines the widget type.
  , fixedListWidgetMainAxisAlignment :: Maybe Text -- ^ The mainAxisAlignment property supports the following options.
  , fixedListWidgetCrossAxisAlignment :: Maybe Text -- ^ The crossAxisAlignment property supports the following options
  , fixedListWidgetComponents :: Maybe [Value] -- ^ An array of child components that you want to make available inside this widget.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FixedListWidget where
  parseJSON = genericParseJSON optionsFixedListWidget
instance ToJSON FixedListWidget where
  toJSON = genericToJSON optionsFixedListWidget

optionsFixedListWidget :: Options
optionsFixedListWidget =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fixedListWidgetType", "type")
      , ("fixedListWidgetMainAxisAlignment", "mainAxisAlignment")
      , ("fixedListWidgetCrossAxisAlignment", "crossAxisAlignment")
      , ("fixedListWidgetComponents", "components")
      ]


-- | 
data Form = Form
  { formId :: Maybe Int -- ^ The identifier for a campaign.
  , formDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , formName :: Maybe Text -- ^ The name of the campaign.
  , formType :: Maybe Text -- ^ The type of campaign trigger. **Sunsetting on March 30, 2025**
  , formCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , formUpdated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , formActive :: Maybe Bool -- ^ If true, the campaign is active and can still send messages.
  , formState :: Maybe Text -- ^ The status of the campaign.
  , formActions :: Maybe [SegmentActionsInner] -- ^ An array of actions contained within the campaign.
  , formFirstUnderscorestarted :: Maybe Int -- ^ The date and time when you first started the campaign and it first became eligible to be triggered.
  , formTags :: Maybe [Text] -- ^ An array of tags you set on this campaign.
  , formFilterUnderscoresegmentUnderscoreids :: Maybe [Int] -- ^ A list of segments used in the campaign filter, returned if the campaign audience was filtered on one or more segments.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Form where
  parseJSON = genericParseJSON optionsForm
instance ToJSON Form where
  toJSON = genericToJSON optionsForm

optionsForm :: Options
optionsForm =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("formId", "id")
      , ("formDeduplicateUnderscoreid", "deduplicate_id")
      , ("formName", "name")
      , ("formType", "type")
      , ("formCreated", "created")
      , ("formUpdated", "updated")
      , ("formActive", "active")
      , ("formState", "state")
      , ("formActions", "actions")
      , ("formFirstUnderscorestarted", "first_started")
      , ("formTags", "tags")
      , ("formFilterUnderscoresegmentUnderscoreids", "filter_segment_ids")
      ]


-- | 
data GetRegion200Response = GetRegion200Response
  { getRegion200ResponseUrl :: Maybe Text -- ^ The URL you will use for future Track API calls.
  , getRegion200ResponseRegion :: Maybe Text -- ^ The 'region' that your account is in. While the `url` relates to the Track API, this tells you which region of the Customer.io API to use. If this value is `eu`, you can append the subdomain any of the Customer.io API URLs with `-eu` to route your request to the appropriate region.
  , getRegion200ResponseEnvironmentUnderscoreid :: Maybe Int -- ^ The identifier for the workspace that your credentials grant access to.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetRegion200Response where
  parseJSON = genericParseJSON optionsGetRegion200Response
instance ToJSON GetRegion200Response where
  toJSON = genericToJSON optionsGetRegion200Response

optionsGetRegion200Response :: Options
optionsGetRegion200Response =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("getRegion200ResponseUrl", "url")
      , ("getRegion200ResponseRegion", "region")
      , ("getRegion200ResponseEnvironmentUnderscoreid", "environment_id")
      ]


-- | 
data ID = ID
  { iDId :: Maybe Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ID where
  parseJSON = genericParseJSON optionsID
instance ToJSON ID where
  toJSON = genericToJSON optionsID

optionsID :: Options
optionsID =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("iDId", "id")
      ]


-- | An array of customer ids that you want to send the broadcast to. These IDs must already exist; your request cannot create a new person.
data IDs = IDs
  { iDsIds :: [Text] -- ^ An array of IDs you want to send a broadcast to. **NOTE**: If your workspace identifies people by `email`, don't use this option. Identify your audience by `emails` instead. 
  , iDsData :: Maybe (Map.Map String Value) -- ^ Contains information you want to use to populate your broadcast.
  , iDsEmailUnderscoreaddUnderscoreduplicates :: Maybe Bool -- ^ an email address associated with more than one profile id is an error.
  , iDsEmailUnderscoreignoreUnderscoremissing :: Maybe Bool -- ^ If false a missing email address is an error.
  , iDsIdUnderscoreignoreUnderscoremissing :: Maybe Bool -- ^ If false, a missing customer ID is an error.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IDs where
  parseJSON = genericParseJSON optionsIDs
instance ToJSON IDs where
  toJSON = genericToJSON optionsIDs

optionsIDs :: Options
optionsIDs =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("iDsIds", "ids")
      , ("iDsData", "data")
      , ("iDsEmailUnderscoreaddUnderscoreduplicates", "email_add_duplicates")
      , ("iDsEmailUnderscoreignoreUnderscoremissing", "email_ignore_missing")
      , ("iDsIdUnderscoreignoreUnderscoremissing", "id_ignore_missing")
      ]


-- | Displays an icon from an icon font that you&#39;ve loaded into your app or project. You must load your icon font in the assets section of your app configuration.
data IconWidget = IconWidget
  { iconWidgetType :: Text -- ^ Defines the widget type.
  , iconWidgetColor :: Text -- ^ The color you want to use for this content. You can only set values here that are defined under [**Content** > **In-App Messages**](https://fly.customer.io/env/last/in-app-messages).
  , iconWidgetFont :: Maybe Value -- ^ The icon font you want to use.
  , iconWidgetSize :: Maybe Int -- ^ The pixel size of the icon.
  , iconWidgetValue :: Text -- ^ The value of the icon that you want to use. For example, for font-awesome, you'd use the name of the icon.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IconWidget where
  parseJSON = genericParseJSON optionsIconWidget
instance ToJSON IconWidget where
  toJSON = genericToJSON optionsIconWidget

optionsIconWidget :: Options
optionsIconWidget =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("iconWidgetType", "type")
      , ("iconWidgetColor", "color")
      , ("iconWidgetFont", "font")
      , ("iconWidgetSize", "size")
      , ("iconWidgetValue", "value")
      ]


-- | 
data Id = Id
  { idId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Id where
  parseJSON = genericParseJSON optionsId
instance ToJSON Id where
  toJSON = genericToJSON optionsId

optionsId :: Options
optionsId =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("idId", "id")
      ]


-- | 
data Id1 = Id1
  { id1Id :: Maybe Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Id1 where
  parseJSON = genericParseJSON optionsId1
instance ToJSON Id1 where
  toJSON = genericToJSON optionsId1

optionsId1 :: Options
optionsId1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("id1Id", "id")
      ]


-- | 
data Id2 = Id2
  { id2Id :: Text -- ^ The identifier for the person represented by the transactional message. **NOTE**: If your workspace identifies people by email, use the `email` identifier instead. 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Id2 where
  parseJSON = genericParseJSON optionsId2
instance ToJSON Id2 where
  toJSON = genericToJSON optionsId2

optionsId2 :: Options
optionsId2 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("id2Id", "id")
      ]


-- | Add or update a person.
data Identify = Identify
  { identifyType :: Text -- ^ The operation modifies a person in Customer.io
  , identifyIdentifiers :: IdentifyAllOfIdentifiers -- ^ 
  , identifyAction :: Text -- ^ Indicates that the operation will `identify` the the item of the specified `type`.
  , identifyAttributes :: Maybe IdentifyAllOfAttributes -- ^ 
  , identifyCioUnderscorerelationships :: Maybe [IdentifyRequestCioRelationshipsRelationshipsInner] -- ^ Each object in the array represents a relationship you want to add to, or remove from, a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Identify where
  parseJSON = genericParseJSON optionsIdentify
instance ToJSON Identify where
  toJSON = genericToJSON optionsIdentify

optionsIdentify :: Options
optionsIdentify =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyType", "type")
      , ("identifyIdentifiers", "identifiers")
      , ("identifyAction", "action")
      , ("identifyAttributes", "attributes")
      , ("identifyCioUnderscorerelationships", "cio_relationships")
      ]


-- | The &#x60;action&#x60; determines the type of operation you want to perform with an object. If &#x60;identifiers.object_id&#x60; does not exist, we&#39;ll create a new object; if it exists, we&#39;ll update the object accordingly. 
data Identify1 = Identify1
  { identify1Identifiers :: Identify1AllOfIdentifiers -- ^ 
  , identify1Type :: Text -- ^ The operation modifies a single object—non person data.
  , identify1Action :: Text -- ^ Indicates that the operation will `identify` the the item of the specified `type`.
  , identify1Attributes :: Maybe Value -- ^ The data that belongs to the object. This is information you might want to associate with people later (through `cio_relationships`). Passing `null` or an empty string removes the attribute from the object. Some attributes have special meaning. Please refer to the list of [reserved attributes](/journeys/objects-create/#reserved-attributes). 
  , identify1CioUnderscorerelationships :: Maybe [Identify1AllOfCioRelationshipsInner] -- ^ The people you want to associate with an object. Each object in the array represents a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Identify1 where
  parseJSON = genericParseJSON optionsIdentify1
instance ToJSON Identify1 where
  toJSON = genericToJSON optionsIdentify1

optionsIdentify1 :: Options
optionsIdentify1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identify1Identifiers", "identifiers")
      , ("identify1Type", "type")
      , ("identify1Action", "action")
      , ("identify1Attributes", "attributes")
      , ("identify1CioUnderscorerelationships", "cio_relationships")
      ]


-- | 
data Identify1AllOfCioRelationshipsInner = Identify1AllOfCioRelationshipsInner
  { identify1AllOfCioRelationshipsInnerIdentifiers :: Maybe Identify1AllOfCioRelationshipsInnerIdentifiers -- ^ 
  , identify1AllOfCioRelationshipsInnerRelationshipUnderscoreattributes :: Maybe (Map.Map String Value) -- ^ The attributes associated with a relationship. Passing null or an empty string removes the attribute from the relationship. 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Identify1AllOfCioRelationshipsInner where
  parseJSON = genericParseJSON optionsIdentify1AllOfCioRelationshipsInner
instance ToJSON Identify1AllOfCioRelationshipsInner where
  toJSON = genericToJSON optionsIdentify1AllOfCioRelationshipsInner

optionsIdentify1AllOfCioRelationshipsInner :: Options
optionsIdentify1AllOfCioRelationshipsInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identify1AllOfCioRelationshipsInnerIdentifiers", "identifiers")
      , ("identify1AllOfCioRelationshipsInnerRelationshipUnderscoreattributes", "relationship_attributes")
      ]


-- | 
data Identify1AllOfCioRelationshipsInnerIdentifiers = Identify1AllOfCioRelationshipsInnerIdentifiers
  { identify1AllOfCioRelationshipsInnerIdentifiersId :: Maybe Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , identify1AllOfCioRelationshipsInnerIdentifiersEmail :: Maybe Text -- ^ The email address of the customer.
  , identify1AllOfCioRelationshipsInnerIdentifiersCioUnderscoreid :: Maybe Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Identify1AllOfCioRelationshipsInnerIdentifiers where
  parseJSON = genericParseJSON optionsIdentify1AllOfCioRelationshipsInnerIdentifiers
instance ToJSON Identify1AllOfCioRelationshipsInnerIdentifiers where
  toJSON = genericToJSON optionsIdentify1AllOfCioRelationshipsInnerIdentifiers

optionsIdentify1AllOfCioRelationshipsInnerIdentifiers :: Options
optionsIdentify1AllOfCioRelationshipsInnerIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identify1AllOfCioRelationshipsInnerIdentifiersId", "id")
      , ("identify1AllOfCioRelationshipsInnerIdentifiersEmail", "email")
      , ("identify1AllOfCioRelationshipsInnerIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | The identifiers for a custom object. When identifying a new object, you *must* use both the &#x60;object_type_id&#x60; and &#x60;object_id&#x60; (where &#x60;object_type_id&#x60; is an integer representing the type of object and the &#x60;object_id&#x60; is the individual identifier for the object).  If you&#39;re updating an existing object, you can use either the &#x60;object_type_id&#x60; and &#x60;object_id&#x60; or the &#x60;cio_object_id&#x60; (where &#x60;cio_object_id&#x60; is an immutable unique value that Customer.io sets for an object when you create it). 
data Identify1AllOfIdentifiers = Identify1AllOfIdentifiers
  { identify1AllOfIdentifiersObjectUnderscoretypeUnderscoreid :: Text -- ^ The object type an object belongs to—like \"Companies\" or \"Accounts\". Object type IDs are string-formatted integers that begin at `1` and increment for each new type.
  , identify1AllOfIdentifiersObjectUnderscoreid :: Text -- ^ The unique identifier for an object. If you use an `object_id` that already exists, we'll update the object accordingly.
  , identify1AllOfIdentifiersCioUnderscoreobjectUnderscoreid :: Text -- ^ A unique value that Customer.io sets for an object when you create it. This ID is immutable.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Identify1AllOfIdentifiers where
  parseJSON = genericParseJSON optionsIdentify1AllOfIdentifiers
instance ToJSON Identify1AllOfIdentifiers where
  toJSON = genericToJSON optionsIdentify1AllOfIdentifiers

optionsIdentify1AllOfIdentifiers :: Options
optionsIdentify1AllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identify1AllOfIdentifiersObjectUnderscoretypeUnderscoreid", "object_type_id")
      , ("identify1AllOfIdentifiersObjectUnderscoreid", "object_id")
      , ("identify1AllOfIdentifiersCioUnderscoreobjectUnderscoreid", "cio_object_id")
      ]


-- | Attributes that you want to add or update for this person.
data IdentifyAllOfAttributes = IdentifyAllOfAttributes
  { identifyAllOfAttributesCioUnderscoresubscriptionUnderscorepreferences :: Maybe IdentifyAllOfAttributesCioSubscriptionPreferences -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyAllOfAttributes where
  parseJSON = genericParseJSON optionsIdentifyAllOfAttributes
instance ToJSON IdentifyAllOfAttributes where
  toJSON = genericToJSON optionsIdentifyAllOfAttributes

optionsIdentifyAllOfAttributes :: Options
optionsIdentifyAllOfAttributes =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyAllOfAttributesCioUnderscoresubscriptionUnderscorepreferences", "cio_subscription_preferences")
      ]


-- | Stores your audience&#39;s subscription preferences if you enable our [subscription center](/subscription-center/) feature. These items are set automatically when people use the unsubscribe link in your messages, but you can set preferences outside the subscription flow. To update select topic preferences while preserving those set for other topics, use JSON dot notation &#x60;\&quot;cio_subscription_preferences.topics.topic_&lt;topic ID&gt;\&quot;:&lt;boolean&gt;&#x60;.
data IdentifyAllOfAttributesCioSubscriptionPreferences = IdentifyAllOfAttributesCioSubscriptionPreferences
  { identifyAllOfAttributesCioSubscriptionPreferencesTopics :: Maybe (Map.Map String Bool) -- ^ Contains active topics in your workspace, named `topic_<id>`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyAllOfAttributesCioSubscriptionPreferences where
  parseJSON = genericParseJSON optionsIdentifyAllOfAttributesCioSubscriptionPreferences
instance ToJSON IdentifyAllOfAttributesCioSubscriptionPreferences where
  toJSON = genericToJSON optionsIdentifyAllOfAttributesCioSubscriptionPreferences

optionsIdentifyAllOfAttributesCioSubscriptionPreferences :: Options
optionsIdentifyAllOfAttributesCioSubscriptionPreferences =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyAllOfAttributesCioSubscriptionPreferencesTopics", "topics")
      ]


-- | The person you want to perform an action for—one of either &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. You cannot pass multiple identifiers.
data IdentifyAllOfIdentifiers = IdentifyAllOfIdentifiers
  { identifyAllOfIdentifiersId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , identifyAllOfIdentifiersEmail :: Text -- ^ The email address of the customer.
  , identifyAllOfIdentifiersCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyAllOfIdentifiers where
  parseJSON = genericParseJSON optionsIdentifyAllOfIdentifiers
instance ToJSON IdentifyAllOfIdentifiers where
  toJSON = genericToJSON optionsIdentifyAllOfIdentifiers

optionsIdentifyAllOfIdentifiers :: Options
optionsIdentifyAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyAllOfIdentifiersId", "id")
      , ("identifyAllOfIdentifiersEmail", "email")
      , ("identifyAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | The &#x60;identify_anonymous&#x60; action lets you relate an object to a person who hasn&#39;t yet identified themselves by anonymous_id. When you identify the person, their anonymous relationship will carry over to the identified profile.
data IdentifyAnonymous = IdentifyAnonymous
  { identifyAnonymousIdentifiers :: Identify1AllOfIdentifiers -- ^ 
  , identifyAnonymousType :: Text -- ^ The operation modifies a single object—non person data.
  , identifyAnonymousAction :: Text -- ^ Indicates that the operation will `identify` the item of the specified `type` and relate it to an `anonymous_id`.
  , identifyAnonymousAttributes :: Maybe Value -- ^ The data that belongs to the object. This is information you might want to associate with people later (through `cio_relationships`). Passing `null` or an empty string removes the attribute from the object. Some attributes have special meaning. Please refer to the list of [reserved attributes](/journeys/objects-create/#reserved-attributes). 
  , identifyAnonymousCioUnderscorerelationships :: Maybe [IdentifyAnonymousAllOfCioRelationshipsInner] -- ^ The anonymous people you want to associate with an object. Each object in the array contains an `anonymous_id` representing a person you haven't yet identified by `id` or `email`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyAnonymous where
  parseJSON = genericParseJSON optionsIdentifyAnonymous
instance ToJSON IdentifyAnonymous where
  toJSON = genericToJSON optionsIdentifyAnonymous

optionsIdentifyAnonymous :: Options
optionsIdentifyAnonymous =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyAnonymousIdentifiers", "identifiers")
      , ("identifyAnonymousType", "type")
      , ("identifyAnonymousAction", "action")
      , ("identifyAnonymousAttributes", "attributes")
      , ("identifyAnonymousCioUnderscorerelationships", "cio_relationships")
      ]


-- | 
data IdentifyAnonymousAllOfCioRelationshipsInner = IdentifyAnonymousAllOfCioRelationshipsInner
  { identifyAnonymousAllOfCioRelationshipsInnerIdentifiers :: Maybe IdentifyAnonymousAllOfCioRelationshipsInnerIdentifiers -- ^ 
  , identifyAnonymousAllOfCioRelationshipsInnerRelationshipUnderscoreattributes :: Maybe Value -- ^ Coming October 2023 - The attributes associated with a relationship. Passing null or an empty string removes the attribute from the relationship.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyAnonymousAllOfCioRelationshipsInner where
  parseJSON = genericParseJSON optionsIdentifyAnonymousAllOfCioRelationshipsInner
instance ToJSON IdentifyAnonymousAllOfCioRelationshipsInner where
  toJSON = genericToJSON optionsIdentifyAnonymousAllOfCioRelationshipsInner

optionsIdentifyAnonymousAllOfCioRelationshipsInner :: Options
optionsIdentifyAnonymousAllOfCioRelationshipsInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyAnonymousAllOfCioRelationshipsInnerIdentifiers", "identifiers")
      , ("identifyAnonymousAllOfCioRelationshipsInnerRelationshipUnderscoreattributes", "relationship_attributes")
      ]


-- | 
data IdentifyAnonymousAllOfCioRelationshipsInnerIdentifiers = IdentifyAnonymousAllOfCioRelationshipsInnerIdentifiers
  { identifyAnonymousAllOfCioRelationshipsInnerIdentifiersAnonymousUnderscoreid :: Maybe Text -- ^ An identifier for an anonymous event, like a cookie. If set as an attribute on a person, any events bearing the same anonymous value are associated with this person. This value must be unique and is not reusable.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyAnonymousAllOfCioRelationshipsInnerIdentifiers where
  parseJSON = genericParseJSON optionsIdentifyAnonymousAllOfCioRelationshipsInnerIdentifiers
instance ToJSON IdentifyAnonymousAllOfCioRelationshipsInnerIdentifiers where
  toJSON = genericToJSON optionsIdentifyAnonymousAllOfCioRelationshipsInnerIdentifiers

optionsIdentifyAnonymousAllOfCioRelationshipsInnerIdentifiers :: Options
optionsIdentifyAnonymousAllOfCioRelationshipsInnerIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyAnonymousAllOfCioRelationshipsInnerIdentifiersAnonymousUnderscoreid", "anonymous_id")
      ]


-- | Identify the person who submitted your form by email.
newtype IdentifyByEmail = IdentifyByEmail { unIdentifyByEmail :: (Map.Map Text Text) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | Identify the person who submitted your form by ID.
newtype IdentifyById = IdentifyById { unIdentifyById :: (Map.Map Text Text) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data IdentifyIdentifierParameter = IdentifyIdentifierParameter
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyIdentifierParameter where
  parseJSON = genericParseJSON optionsIdentifyIdentifierParameter
instance ToJSON IdentifyIdentifierParameter where
  toJSON = genericToJSON optionsIdentifyIdentifierParameter

optionsIdentifyIdentifierParameter :: Options
optionsIdentifyIdentifierParameter =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | Add or update a person.
data IdentifyPerson = IdentifyPerson
  { identifyPersonType :: Text -- ^ The operation modifies a person in Customer.io
  , identifyPersonIdentifiers :: IdentifyPersonAllOfIdentifiers -- ^ 
  , identifyPersonAction :: Text -- ^ Indicates that the operation will `identify` the the item of the specified `type`.
  , identifyPersonAttributes :: Maybe IdentifyPersonAllOfAttributes -- ^ 
  , identifyPersonCioUnderscorerelationships :: Maybe [IdentifyPersonAllOfCioRelationships] -- ^ Each object in the array represents a relationship you want to add to, or remove from, a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyPerson where
  parseJSON = genericParseJSON optionsIdentifyPerson
instance ToJSON IdentifyPerson where
  toJSON = genericToJSON optionsIdentifyPerson

optionsIdentifyPerson :: Options
optionsIdentifyPerson =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyPersonType", "type")
      , ("identifyPersonIdentifiers", "identifiers")
      , ("identifyPersonAction", "action")
      , ("identifyPersonAttributes", "attributes")
      , ("identifyPersonCioUnderscorerelationships", "cio_relationships")
      ]


-- | Attributes that you want to add or update for this person.
data IdentifyPersonAllOfAttributes = IdentifyPersonAllOfAttributes
  { identifyPersonAllOfAttributesCioUnderscoresubscriptionUnderscorepreferences :: Maybe IdentifyPersonAllOfAttributesCioSubscriptionPreferences -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyPersonAllOfAttributes where
  parseJSON = genericParseJSON optionsIdentifyPersonAllOfAttributes
instance ToJSON IdentifyPersonAllOfAttributes where
  toJSON = genericToJSON optionsIdentifyPersonAllOfAttributes

optionsIdentifyPersonAllOfAttributes :: Options
optionsIdentifyPersonAllOfAttributes =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyPersonAllOfAttributesCioUnderscoresubscriptionUnderscorepreferences", "cio_subscription_preferences")
      ]


-- | Stores your audience&#39;s subscription preferences if you enable our [subscription center](/subscription-center/) feature. These items are set automatically when people use the unsubscribe link in your messages, but you can set preferences outside the subscription flow. To update select topic preferences while preserving those set for other topics, use JSON dot notation &#x60;\&quot;cio_subscription_preferences.topics.topic_&lt;topic ID&gt;\&quot;:&lt;boolean&gt;&#x60;.
data IdentifyPersonAllOfAttributesCioSubscriptionPreferences = IdentifyPersonAllOfAttributesCioSubscriptionPreferences
  { identifyPersonAllOfAttributesCioSubscriptionPreferencesTopics :: Maybe (Map.Map String Bool) -- ^ Contains active topics in your workspace, named `topic_<id>`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyPersonAllOfAttributesCioSubscriptionPreferences where
  parseJSON = genericParseJSON optionsIdentifyPersonAllOfAttributesCioSubscriptionPreferences
instance ToJSON IdentifyPersonAllOfAttributesCioSubscriptionPreferences where
  toJSON = genericToJSON optionsIdentifyPersonAllOfAttributesCioSubscriptionPreferences

optionsIdentifyPersonAllOfAttributesCioSubscriptionPreferences :: Options
optionsIdentifyPersonAllOfAttributesCioSubscriptionPreferences =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyPersonAllOfAttributesCioSubscriptionPreferencesTopics", "topics")
      ]


-- | 
data IdentifyPersonAllOfCioRelationships = IdentifyPersonAllOfCioRelationships
  { identifyPersonAllOfCioRelationshipsIdentifiers :: Maybe ObjectCommonAllOfIdentifiers -- ^ 
  , identifyPersonAllOfCioRelationshipsRelationshipUnderscoreattributes :: Maybe (Map.Map String Value) -- ^ The attributes associated with a relationship. Passing null or an empty string removes the attribute from the relationship. 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyPersonAllOfCioRelationships where
  parseJSON = genericParseJSON optionsIdentifyPersonAllOfCioRelationships
instance ToJSON IdentifyPersonAllOfCioRelationships where
  toJSON = genericToJSON optionsIdentifyPersonAllOfCioRelationships

optionsIdentifyPersonAllOfCioRelationships :: Options
optionsIdentifyPersonAllOfCioRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyPersonAllOfCioRelationshipsIdentifiers", "identifiers")
      , ("identifyPersonAllOfCioRelationshipsRelationshipUnderscoreattributes", "relationship_attributes")
      ]


-- | The person you want to perform an action for—one of either &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. You cannot pass multiple identifiers.
data IdentifyPersonAllOfIdentifiers = IdentifyPersonAllOfIdentifiers
  { identifyPersonAllOfIdentifiersId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , identifyPersonAllOfIdentifiersEmail :: Text -- ^ The email address of the customer.
  , identifyPersonAllOfIdentifiersCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyPersonAllOfIdentifiers where
  parseJSON = genericParseJSON optionsIdentifyPersonAllOfIdentifiers
instance ToJSON IdentifyPersonAllOfIdentifiers where
  toJSON = genericToJSON optionsIdentifyPersonAllOfIdentifiers

optionsIdentifyPersonAllOfIdentifiers :: Options
optionsIdentifyPersonAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyPersonAllOfIdentifiersId", "id")
      , ("identifyPersonAllOfIdentifiersEmail", "email")
      , ("identifyPersonAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | The body of the request contains key-value pairs representing attributes that you want to assign to, or update for, a person.  If your request body contains \&quot;identifiers\&quot; (like &#x60;id&#x60; or &#x60;email&#x60;), your request attempts to update that person. If the identifier in the path and identifiers in the request body belong to different people, your request will produce an *Attribute Update Failure*. 
newtype IdentifyRequest = IdentifyRequest { unIdentifyRequest :: (Map.Map Text IdentifyRequestValue) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | Describes relationships to an entity—a non-person object in Customer.io, like a company, educational course, job board, etc.
data IdentifyRequestCioRelationships = IdentifyRequestCioRelationships
  { identifyRequestCioRelationshipsAction :: Maybe Text -- ^ This determines whether the `relationships` array adds relationships to a person or removes them from a person.
  , identifyRequestCioRelationshipsRelationships :: Maybe [IdentifyRequestCioRelationshipsRelationshipsInner] -- ^ Each object in the array represents a relationship you want to add to, or remove from, a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyRequestCioRelationships where
  parseJSON = genericParseJSON optionsIdentifyRequestCioRelationships
instance ToJSON IdentifyRequestCioRelationships where
  toJSON = genericToJSON optionsIdentifyRequestCioRelationships

optionsIdentifyRequestCioRelationships :: Options
optionsIdentifyRequestCioRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyRequestCioRelationshipsAction", "action")
      , ("identifyRequestCioRelationshipsRelationships", "relationships")
      ]


-- | 
data IdentifyRequestCioRelationshipsRelationshipsInner = IdentifyRequestCioRelationshipsRelationshipsInner
  { identifyRequestCioRelationshipsRelationshipsInnerIdentifiers :: Maybe IdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers -- ^ 
  , identifyRequestCioRelationshipsRelationshipsInnerRelationshipUnderscoreattributes :: Maybe (Map.Map String Value) -- ^ The attributes associated with a relationship. Passing null or an empty string removes the attribute from the relationship. 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyRequestCioRelationshipsRelationshipsInner where
  parseJSON = genericParseJSON optionsIdentifyRequestCioRelationshipsRelationshipsInner
instance ToJSON IdentifyRequestCioRelationshipsRelationshipsInner where
  toJSON = genericToJSON optionsIdentifyRequestCioRelationshipsRelationshipsInner

optionsIdentifyRequestCioRelationshipsRelationshipsInner :: Options
optionsIdentifyRequestCioRelationshipsRelationshipsInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyRequestCioRelationshipsRelationshipsInnerIdentifiers", "identifiers")
      , ("identifyRequestCioRelationshipsRelationshipsInnerRelationshipUnderscoreattributes", "relationship_attributes")
      ]


-- | The identifiers for a particular object. You can use either the &#x60;object_type_id&#x60; and &#x60;object_id&#x60; (where &#x60;object_type_id&#x60; represents the type of object and the &#x60;object_id&#x60; is the individual identifier for the object) or the &#x60;cio_object_id&#x60;.
data IdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers = IdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers
  { identifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiersObjectUnderscoretypeUnderscoreid :: Text -- ^ The object type an object belongs to—like \"Companies\" or \"Accounts\". Object type IDs are string-formatted integers that begin at `1` and increment for each new type.
  , identifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiersObjectUnderscoreid :: Text -- ^ The unique identifier for an object. If you use an `object_id` that already exists, we'll update the object accordingly.
  , identifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiersCioUnderscoreobjectUnderscoreid :: Text -- ^ A unique value that Customer.io sets for an object when you create it. This ID is immutable.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers where
  parseJSON = genericParseJSON optionsIdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers
instance ToJSON IdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers where
  toJSON = genericToJSON optionsIdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers

optionsIdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers :: Options
optionsIdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiersObjectUnderscoretypeUnderscoreid", "object_type_id")
      , ("identifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiersObjectUnderscoreid", "object_id")
      , ("identifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiersCioUnderscoreobjectUnderscoreid", "cio_object_id")
      ]


-- | Stores your audience&#39;s subscription preferences if you enable our [subscription center](/subscription-center/) feature. These items are set automatically when people use the unsubscribe link in your messages, but you can set preferences outside the subscription flow. To update select topic preferences while preserving those set for other topics, use JSON dot notation &#x60;\&quot;cio_subscription_preferences.topics.topic_&lt;topic ID&gt;\&quot;:&lt;boolean&gt;&#x60;.
data IdentifyRequestCioSubscriptionPreferences = IdentifyRequestCioSubscriptionPreferences
  { identifyRequestCioSubscriptionPreferencesTopics :: Maybe (Map.Map String Bool) -- ^ Contains active topics in your workspace, named `topic_<id>`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyRequestCioSubscriptionPreferences where
  parseJSON = genericParseJSON optionsIdentifyRequestCioSubscriptionPreferences
instance ToJSON IdentifyRequestCioSubscriptionPreferences where
  toJSON = genericToJSON optionsIdentifyRequestCioSubscriptionPreferences

optionsIdentifyRequestCioSubscriptionPreferences :: Options
optionsIdentifyRequestCioSubscriptionPreferences =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("identifyRequestCioSubscriptionPreferencesTopics", "topics")
      ]


-- | Set attributes on customers. Attributes can have string, integer, or boolean values.
data IdentifyRequestValue = IdentifyRequestValue
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IdentifyRequestValue where
  parseJSON = genericParseJSON optionsIdentifyRequestValue
instance ToJSON IdentifyRequestValue where
  toJSON = genericToJSON optionsIdentifyRequestValue

optionsIdentifyRequestValue :: Options
optionsIdentifyRequestValue =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data Ids = Ids
  { idsId :: Text -- ^ The ID of the recipient.
  , idsData :: Maybe (Map.Map String Value) -- ^ Merge data associated with the recipient.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Ids where
  parseJSON = genericParseJSON optionsIds
instance ToJSON Ids where
  toJSON = genericToJSON optionsIds

optionsIds :: Options
optionsIds =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("idsId", "id")
      , ("idsData", "data")
      ]


-- | Displays an image. You can provide the URL of an image. Or, if you use the template designer, you can upload an image and host it directly.
data ImageWidget = ImageWidget
  { imageWidgetType :: Text -- ^ Defines the widget type.
  , imageWidgetImage :: Value -- ^ The URL of the image that you want ot use in your message.
  , imageWidgetFit :: Maybe Text -- ^ Determines how the image fits your message. Defaults to `cover`.
  , imageWidgetHeight :: Maybe Int -- ^ The height of the component in pixels, if you want to constrain it. If you don't set a height or width, we'll scale your content to fit your message or container.
  , imageWidgetWidth :: Maybe Int -- ^ The width of the component in pixels, if you want to constrain it. If you don't set a height or width, we'll scale your content to fit your message or container.
  , imageWidgetCornerRadius :: Maybe Int -- ^ Sets the radius of corners for an item in pixels, similar to the `border-radius` CSS property.
  , imageWidgetFadeInDuration :: Maybe Int -- ^ The durration for the image to fade in, in milliseconds, similar to the `fadeIn` CSS transition property.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImageWidget where
  parseJSON = genericParseJSON optionsImageWidget
instance ToJSON ImageWidget where
  toJSON = genericToJSON optionsImageWidget

optionsImageWidget :: Options
optionsImageWidget =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("imageWidgetType", "type")
      , ("imageWidgetImage", "image")
      , ("imageWidgetFit", "fit")
      , ("imageWidgetHeight", "height")
      , ("imageWidgetWidth", "width")
      , ("imageWidgetCornerRadius", "cornerRadius")
      , ("imageWidgetFadeInDuration", "fadeInDuration")
      ]


-- | The type of identifier you want to use to identify people in your sheet—&#x60;id&#x60; or &#x60;email&#x60;. At least one column in the CSV must contain an identifier.
data ImportIdentifier = ImportIdentifier
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImportIdentifier where
  parseJSON = genericParseJSON optionsImportIdentifier
instance ToJSON ImportIdentifier where
  toJSON = genericToJSON optionsImportIdentifier

optionsImportIdentifier :: Options
optionsImportIdentifier =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | Represents an import operation.
data ImportObject = ImportObject
  { importObjectId :: Maybe Int -- ^ This is the `import_id` you'll use if you want to [lookup your import operation](#getImport).
  , importObjectCreatedUnderscoreat :: Maybe Int -- ^ The date time when the referenced ID was created.
  , importObjectUpdatedUnderscoreat :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , importObjectName :: Maybe Text -- ^ A friendly name for your import. This helps you identify your import.
  , importObjectDescription :: Maybe Text -- ^ A helpful description that can help you find and recognize your import operation.
  , importObjectRowsUnderscoretoUnderscoreimport :: Maybe Int -- ^ The total number of importable rows we found in the CSV.
  , importObjectRowsUnderscoreimported :: Maybe Int -- ^ The number of rows we imported from the CSV.
  , importObjectState :: Maybe Text -- ^ The state of the import—whether your import is being processed, fully completed (`imported`), or if it failed.
  , importObjectType :: Maybe Text -- ^ The type of import.
  , importObjectIdentifier :: Maybe Text -- ^ The type of identifier you used to identify people in your CSV. Not applicable for object imports.
  , importObjectDataUnderscoretoUnderscoreprocess :: Maybe Text -- ^ Determines whether your import operation performs `all` add/update operations, only adds items (`only_new`), or only updates existing items (`only_existing`). Defaults to `all`. If `import_type` is `event`, you can only use `all` or `only_existing`.   This field was previously called `people_to_process` - we still support it but will deprecate it soon. 
  , importObjectPeopleUnderscoretoUnderscoreprocess :: Maybe Text -- ^ Returned for people and event imports, even if you imported using the field `data_to_process`. This field will be deprecated soon. 
  , importObjectObjectUnderscoretypeUnderscoreid :: Maybe Text -- ^ The object type an object belongs to—like \"Companies\" or \"Accounts\". Only applies to object imports.
  , importObjectError :: Maybe Text -- ^ If your import fails, this helps you understand why.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImportObject where
  parseJSON = genericParseJSON optionsImportObject
instance ToJSON ImportObject where
  toJSON = genericToJSON optionsImportObject

optionsImportObject :: Options
optionsImportObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("importObjectId", "id")
      , ("importObjectCreatedUnderscoreat", "created_at")
      , ("importObjectUpdatedUnderscoreat", "updated_at")
      , ("importObjectName", "name")
      , ("importObjectDescription", "description")
      , ("importObjectRowsUnderscoretoUnderscoreimport", "rows_to_import")
      , ("importObjectRowsUnderscoreimported", "rows_imported")
      , ("importObjectState", "state")
      , ("importObjectType", "type")
      , ("importObjectIdentifier", "identifier")
      , ("importObjectDataUnderscoretoUnderscoreprocess", "data_to_process")
      , ("importObjectPeopleUnderscoretoUnderscoreprocess", "people_to_process")
      , ("importObjectObjectUnderscoretypeUnderscoreid", "object_type_id")
      , ("importObjectError", "error")
      ]


-- | 
data ImportRequest = ImportRequest
  { importRequestName :: Text -- ^ A friendly name for your import. This helps you identify your import.
  , importRequestDataUnderscorefileUnderscoreurl :: Text -- ^ The URL or path to the CSV file you want to import.
  , importRequestType :: Text -- ^ The type of import.
  , importRequestIdentifier :: Text -- ^ The type of identifier you want to use to identify people in your sheet—`id` or `email`. At least one column in the CSV must contain an identifier.
  , importRequestDataUnderscoretoUnderscoreprocess :: Maybe Text -- ^ Determines whether your import operation performs `all` add/update operations, only adds items (`only_new`), or only updates existing items (`only_existing`). Defaults to `all`. If `import_type` is `event`, you can only use `all` or `only_existing`.   This field was previously called `people_to_process` - we still support it but will deprecate it soon. 
  , importRequestDescription :: Maybe Text -- ^ A helpful description that can help you find and recognize your import operation.
  , importRequestObjectUnderscoretypeUnderscoreid :: Text -- ^ The object type an object belongs to—like \"Companies\" or \"Accounts\". Object type IDs are string-formatted integers that begin at `1` and increment for each new type.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImportRequest where
  parseJSON = genericParseJSON optionsImportRequest
instance ToJSON ImportRequest where
  toJSON = genericToJSON optionsImportRequest

optionsImportRequest :: Options
optionsImportRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("importRequestName", "name")
      , ("importRequestDataUnderscorefileUnderscoreurl", "data_file_url")
      , ("importRequestType", "type")
      , ("importRequestIdentifier", "identifier")
      , ("importRequestDataUnderscoretoUnderscoreprocess", "data_to_process")
      , ("importRequestDescription", "description")
      , ("importRequestObjectUnderscoretypeUnderscoreid", "object_type_id")
      ]


-- | The state of the import—whether your import is being processed, fully completed (&#x60;imported&#x60;), or if it failed.
data ImportState = ImportState
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImportState where
  parseJSON = genericParseJSON optionsImportState
instance ToJSON ImportState where
  toJSON = genericToJSON optionsImportState

optionsImportState :: Options
optionsImportState =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | The type of import.
data ImportType = ImportType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ImportType where
  parseJSON = genericParseJSON optionsImportType
instance ToJSON ImportType where
  toJSON = genericToJSON optionsImportType

optionsImportType :: Options
optionsImportType =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data InApp = InApp
  { inAppDeliveryUnderscoreid :: Text -- ^ The CIO-Delivery-ID from the notification that you want to associate the `event` with.
  , inAppTimestamp :: Maybe Int -- ^ The unix timestamp when the event occurred.
  , inAppMetric :: Text -- ^ The type of device-side event you want to report back to Customer.io.
  , inAppRecipient :: Maybe Text -- ^ The email address or ID of the recipient (depending on the value you use to target in-app messages).
  , inAppHref :: Maybe Text -- ^ For `clicked` metrics, this is the link the recipient clicked.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InApp where
  parseJSON = genericParseJSON optionsInApp
instance ToJSON InApp where
  toJSON = genericToJSON optionsInApp

optionsInApp :: Options
optionsInApp =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("inAppDeliveryUnderscoreid", "delivery_id")
      , ("inAppTimestamp", "timestamp")
      , ("inAppMetric", "metric")
      , ("inAppRecipient", "recipient")
      , ("inAppHref", "href")
      ]


-- | Event object passed to the listener function when user performs an action in the in-app message.
data InAppMessageActionEvent = InAppMessageActionEvent
  { inAppMessageActionEventType :: Maybe Text -- ^ Defines the event type.
  , inAppMessageActionEventDetail :: Maybe InAppMessageActionEventDetail -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InAppMessageActionEvent where
  parseJSON = genericParseJSON optionsInAppMessageActionEvent
instance ToJSON InAppMessageActionEvent where
  toJSON = genericToJSON optionsInAppMessageActionEvent

optionsInAppMessageActionEvent :: Options
optionsInAppMessageActionEvent =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("inAppMessageActionEventType", "type")
      , ("inAppMessageActionEventDetail", "detail")
      ]


-- | 
data InAppMessageActionEventDetail = InAppMessageActionEventDetail
  { inAppMessageActionEventDetailMessageId :: Maybe Text -- ^ Identifier string of the in-app message.
  , inAppMessageActionEventDetailDeliveryId :: Maybe Text -- ^ Delivery Id for the corresponding in-app message (not present in test message).
  , inAppMessageActionEventDetailActionName :: Maybe Text -- ^ The name of the action specified when building the in-app message.
  , inAppMessageActionEventDetailActionValue :: Maybe Text -- ^ The type of action that triggered the event.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InAppMessageActionEventDetail where
  parseJSON = genericParseJSON optionsInAppMessageActionEventDetail
instance ToJSON InAppMessageActionEventDetail where
  toJSON = genericToJSON optionsInAppMessageActionEventDetail

optionsInAppMessageActionEventDetail :: Options
optionsInAppMessageActionEventDetail =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("inAppMessageActionEventDetailMessageId", "messageId")
      , ("inAppMessageActionEventDetailDeliveryId", "deliveryId")
      , ("inAppMessageActionEventDetailActionName", "actionName")
      , ("inAppMessageActionEventDetailActionValue", "actionValue")
      ]


-- | Event object passed to the listener function when user dismisses an in-app message.
data InAppMessageDismissedEvent = InAppMessageDismissedEvent
  { inAppMessageDismissedEventType :: Maybe Text -- ^ Defines the event type.
  , inAppMessageDismissedEventDetail :: Maybe InAppMessageOpenedEventDetail -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InAppMessageDismissedEvent where
  parseJSON = genericParseJSON optionsInAppMessageDismissedEvent
instance ToJSON InAppMessageDismissedEvent where
  toJSON = genericToJSON optionsInAppMessageDismissedEvent

optionsInAppMessageDismissedEvent :: Options
optionsInAppMessageDismissedEvent =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("inAppMessageDismissedEventType", "type")
      , ("inAppMessageDismissedEventDetail", "detail")
      ]


-- | Event object passed to the listener function when an error occurs.
data InAppMessageErrorEvent = InAppMessageErrorEvent
  { inAppMessageErrorEventType :: Maybe Text -- ^ Defines the event type.
  , inAppMessageErrorEventDetail :: Maybe InAppMessageOpenedEventDetail -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InAppMessageErrorEvent where
  parseJSON = genericParseJSON optionsInAppMessageErrorEvent
instance ToJSON InAppMessageErrorEvent where
  toJSON = genericToJSON optionsInAppMessageErrorEvent

optionsInAppMessageErrorEvent :: Options
optionsInAppMessageErrorEvent =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("inAppMessageErrorEventType", "type")
      , ("inAppMessageErrorEventDetail", "detail")
      ]


-- | An object containing message specific data depending on the event type.
data InAppMessageEventDetail = InAppMessageEventDetail
  { inAppMessageEventDetailMessageId :: Maybe Text -- ^ Identifier string of the in-app message.
  , inAppMessageEventDetailDeliveryId :: Maybe Text -- ^ Delivery Id for the corresponding in-app message (not present in test message).
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InAppMessageEventDetail where
  parseJSON = genericParseJSON optionsInAppMessageEventDetail
instance ToJSON InAppMessageEventDetail where
  toJSON = genericToJSON optionsInAppMessageEventDetail

optionsInAppMessageEventDetail :: Options
optionsInAppMessageEventDetail =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("inAppMessageEventDetailMessageId", "messageId")
      , ("inAppMessageEventDetailDeliveryId", "deliveryId")
      ]


-- | Event object passed to the listener function when user is shown an in-app message.
data InAppMessageOpenedEvent = InAppMessageOpenedEvent
  { inAppMessageOpenedEventType :: Maybe Text -- ^ Defines the event type.
  , inAppMessageOpenedEventDetail :: Maybe InAppMessageOpenedEventDetail -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InAppMessageOpenedEvent where
  parseJSON = genericParseJSON optionsInAppMessageOpenedEvent
instance ToJSON InAppMessageOpenedEvent where
  toJSON = genericToJSON optionsInAppMessageOpenedEvent

optionsInAppMessageOpenedEvent :: Options
optionsInAppMessageOpenedEvent =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("inAppMessageOpenedEventType", "type")
      , ("inAppMessageOpenedEventDetail", "detail")
      ]


-- | An object containing message specific data depending on the event type.
data InAppMessageOpenedEventDetail = InAppMessageOpenedEventDetail
  { inAppMessageOpenedEventDetailMessageId :: Maybe Text -- ^ Identifier string of the in-app message.
  , inAppMessageOpenedEventDetailDeliveryId :: Maybe Text -- ^ Delivery Id for the corresponding in-app message (not present in test message).
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InAppMessageOpenedEventDetail where
  parseJSON = genericParseJSON optionsInAppMessageOpenedEventDetail
instance ToJSON InAppMessageOpenedEventDetail where
  toJSON = genericToJSON optionsInAppMessageOpenedEventDetail

optionsInAppMessageOpenedEventDetail :: Options
optionsInAppMessageOpenedEventDetail =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("inAppMessageOpenedEventDetailMessageId", "messageId")
      , ("inAppMessageOpenedEventDetailDeliveryId", "deliveryId")
      ]


-- | Your payload changes depending on whether you send to iOS devices through Google&#39;s Firebase Cloud Messaging (FCM) or Apple&#39;s Push Notification service (APNs).
data IosFcmAndApns = IosFcmAndApns
  { iosFcmAndApnsMessage :: FCMMessage -- ^ 
  , iosFcmAndApnsCIO :: Maybe APNSCIO -- ^ 
  , iosFcmAndApnsAps :: Maybe FCMMessageApnsPayloadAps -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IosFcmAndApns where
  parseJSON = genericParseJSON optionsIosFcmAndApns
instance ToJSON IosFcmAndApns where
  toJSON = genericToJSON optionsIosFcmAndApns

optionsIosFcmAndApns :: Options
optionsIosFcmAndApns =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("iosFcmAndApnsMessage", "message")
      , ("iosFcmAndApnsCIO", "CIO")
      , ("iosFcmAndApnsAps", "aps")
      ]


-- | A push payload intended for an iOS device.
data IosSharedOptions = IosSharedOptions
  { iosSharedOptionsAlert :: Maybe FCMMessageApnsPayloadApsAlert -- ^ 
  , iosSharedOptionsBadge :: Maybe Int -- ^ The number you want to display on your app's icon. Set to 0 to remove the current badge, if any.
  , iosSharedOptionsSound :: Maybe FCMMessageApnsPayloadApsSound -- ^ 
  , iosSharedOptionsThreadDashid :: Maybe Text -- ^ An identifier to group related notifications.
  , iosSharedOptionsCategory :: Maybe Text -- ^ The notification’s type. This string must correspond to the identifier of one of the `UNNotificationCategory` objects you register at launch time.
  , iosSharedOptionsContentDashavailable :: Maybe Int -- ^ The background notification flag. Use `1` without an `alert` to perform a silent update. `0` indicates a normal push notification.
  , iosSharedOptionsMutableDashcontent :: Maybe Int -- ^ If you use the Customer.io SDK, you *must* set this value to `1` to support images and \"delivered\" metrics from your push notifications. When the value is 1, your notification is passed to your notification service app extension before delivery. Use your extension to modify the notification’s content. 
  , iosSharedOptionsTargetDashcontentDashid :: Maybe Text -- ^ The identifier of the window brought forward.
  , iosSharedOptionsInterruptionDashlevel :: Maybe Text -- ^ Indicates the importance and delivery timing of a notification.
  , iosSharedOptionsRelevanceDashscore :: Maybe Double -- ^ A number between 0 and 1. The highest score is considered the \"most relevant\"  and is featured in the notification summary.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON IosSharedOptions where
  parseJSON = genericParseJSON optionsIosSharedOptions
instance ToJSON IosSharedOptions where
  toJSON = genericToJSON optionsIosSharedOptions

optionsIosSharedOptions :: Options
optionsIosSharedOptions =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("iosSharedOptionsAlert", "alert")
      , ("iosSharedOptionsBadge", "badge")
      , ("iosSharedOptionsSound", "sound")
      , ("iosSharedOptionsThreadDashid", "thread-id")
      , ("iosSharedOptionsCategory", "category")
      , ("iosSharedOptionsContentDashavailable", "content-available")
      , ("iosSharedOptionsMutableDashcontent", "mutable-content")
      , ("iosSharedOptionsTargetDashcontentDashid", "target-content-id")
      , ("iosSharedOptionsInterruptionDashlevel", "interruption-level")
      , ("iosSharedOptionsRelevanceDashscore", "relevance-score")
      ]


-- | 
data LinkMetrics = LinkMetrics
  { linkMetricsLink :: Maybe LinkMetricsLink -- ^ 
  , linkMetricsMetric :: Maybe LinkMetricsMetric -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LinkMetrics where
  parseJSON = genericParseJSON optionsLinkMetrics
instance ToJSON LinkMetrics where
  toJSON = genericToJSON optionsLinkMetrics

optionsLinkMetrics :: Options
optionsLinkMetrics =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("linkMetricsLink", "link")
      , ("linkMetricsMetric", "metric")
      ]


-- | 
data LinkMetricsLink = LinkMetricsLink
  { linkMetricsLinkId :: Maybe Int -- ^ The ID of the link.
  , linkMetricsLinkHref :: Maybe Text -- ^ The link destination—a URL, mailto, etc.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LinkMetricsLink where
  parseJSON = genericParseJSON optionsLinkMetricsLink
instance ToJSON LinkMetricsLink where
  toJSON = genericToJSON optionsLinkMetricsLink

optionsLinkMetricsLink :: Options
optionsLinkMetricsLink =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("linkMetricsLinkId", "id")
      , ("linkMetricsLinkHref", "href")
      ]


-- | Contains metrics for the link.
data LinkMetricsMetric = LinkMetricsMetric
  { linkMetricsMetricSeries :: Maybe LinkMetricsMetricSeries -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LinkMetricsMetric where
  parseJSON = genericParseJSON optionsLinkMetricsMetric
instance ToJSON LinkMetricsMetric where
  toJSON = genericToJSON optionsLinkMetricsMetric

optionsLinkMetricsMetric :: Options
optionsLinkMetricsMetric =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("linkMetricsMetricSeries", "series")
      ]


-- | 
data LinkMetricsMetricSeries = LinkMetricsMetricSeries
  { linkMetricsMetricSeriesClicked :: Maybe [Int] -- ^ An array of results from oldest to newest, where each result indicates a period.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON LinkMetricsMetricSeries where
  parseJSON = genericParseJSON optionsLinkMetricsMetricSeries
instance ToJSON LinkMetricsMetricSeries where
  toJSON = genericToJSON optionsLinkMetricsMetricSeries

optionsLinkMetricsMetricSeries :: Options
optionsLinkMetricsMetricSeries =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("linkMetricsMetricSeriesClicked", "clicked")
      ]


-- | 
data Merge400Response = Merge400Response
  { merge400ResponseMeta :: Maybe Merge400ResponseMeta -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Merge400Response where
  parseJSON = genericParseJSON optionsMerge400Response
instance ToJSON Merge400Response where
  toJSON = genericToJSON optionsMerge400Response

optionsMerge400Response :: Options
optionsMerge400Response =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("merge400ResponseMeta", "meta")
      ]


-- | Contains errors.
data Merge400ResponseMeta = Merge400ResponseMeta
  { merge400ResponseMetaError :: Maybe Text -- ^ Describes the error that caused your request to fail.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Merge400ResponseMeta where
  parseJSON = genericParseJSON optionsMerge400ResponseMeta
instance ToJSON Merge400ResponseMeta where
  toJSON = genericToJSON optionsMerge400ResponseMeta

optionsMerge400ResponseMeta :: Options
optionsMerge400ResponseMeta =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("merge400ResponseMetaError", "error")
      ]


-- | Provide identifiers for the &#x60;primary&#x60; and &#x60;secondary&#x60; people you want to merge together.
data MergeRequest = MergeRequest
  { mergeRequestPrimary :: MergeRequestPrimary -- ^ 
  , mergeRequestSecondary :: MergeRequestSecondary -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MergeRequest where
  parseJSON = genericParseJSON optionsMergeRequest
instance ToJSON MergeRequest where
  toJSON = genericToJSON optionsMergeRequest

optionsMergeRequest :: Options
optionsMergeRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("mergeRequestPrimary", "primary")
      , ("mergeRequestSecondary", "secondary")
      ]


-- | The person that you want to remain after the merge, identified by one of &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. This person receives information from the secondary person in the merge.   If email is disabled as an identifier in your [workspace settings](https://fly.customer.io/workspaces/last/settings/edit), then you must reference people by &#x60;id&#x60; or &#x60;cio_id&#x60;. Under How to Modify, &#x60;id&#x60; must be set to \&quot;Reference people by cio_id\&quot; for a successful merge.  
data MergeRequestPrimary = MergeRequestPrimary
  { mergeRequestPrimaryId :: Maybe Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , mergeRequestPrimaryEmail :: Maybe Text -- ^ The email address of the customer.
  , mergeRequestPrimaryCioUnderscoreid :: Maybe Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MergeRequestPrimary where
  parseJSON = genericParseJSON optionsMergeRequestPrimary
instance ToJSON MergeRequestPrimary where
  toJSON = genericToJSON optionsMergeRequestPrimary

optionsMergeRequestPrimary :: Options
optionsMergeRequestPrimary =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("mergeRequestPrimaryId", "id")
      , ("mergeRequestPrimaryEmail", "email")
      , ("mergeRequestPrimaryCioUnderscoreid", "cio_id")
      ]


-- | The person that you want to delete after the merge, identified by one of &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. This person&#39;s information is merged into the primary person&#39;s profile and then it is deleted.  If email is disabled as an identifier in your [workspace settings](https://fly.customer.io/workspaces/last/settings/edit), then you must reference people by &#x60;id&#x60; or &#x60;cio_id&#x60;. Under How to Modify, &#x60;id&#x60; must be set to \&quot;Reference people by cio_id\&quot; for a successful merge. 
data MergeRequestSecondary = MergeRequestSecondary
  { mergeRequestSecondaryId :: Maybe Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , mergeRequestSecondaryEmail :: Maybe Text -- ^ The email address of the customer.
  , mergeRequestSecondaryCioUnderscoreid :: Maybe Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MergeRequestSecondary where
  parseJSON = genericParseJSON optionsMergeRequestSecondary
instance ToJSON MergeRequestSecondary where
  toJSON = genericToJSON optionsMergeRequestSecondary

optionsMergeRequestSecondary :: Options
optionsMergeRequestSecondary =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("mergeRequestSecondaryId", "id")
      , ("mergeRequestSecondaryEmail", "email")
      , ("mergeRequestSecondaryCioUnderscoreid", "cio_id")
      ]


-- | 
data MessageDelivery = MessageDelivery
  { messageDeliveryDelivered :: Maybe Int -- ^ The date-time when the message was delivered, if applicable.
  , messageDeliveryDeliveryUnderscoreid :: Maybe Text -- ^ The message ID.
  , messageDeliveryOpened :: Maybe Bool -- ^ Indicates whether or not a customer opened a message, if the message was delivered.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MessageDelivery where
  parseJSON = genericParseJSON optionsMessageDelivery
instance ToJSON MessageDelivery where
  toJSON = genericToJSON optionsMessageDelivery

optionsMessageDelivery :: Options
optionsMessageDelivery =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("messageDeliveryDelivered", "delivered")
      , ("messageDeliveryDeliveryUnderscoreid", "delivery_id")
      , ("messageDeliveryOpened", "opened")
      ]


-- | Metrics for an individual instance of a message; each item in the object represents the timestamp when a message achieved a particular metric. This object only contains metrics that have been recorded.
data MessageMetrics = MessageMetrics
  { messageMetricsBounced :: Maybe Int -- ^ The timestamp when the message `bounced`.
  , messageMetricsClicked :: Maybe Int -- ^ The timestamp when the message was `clicked`.
  , messageMetricsHumanUnderscoreclicked :: Maybe Int -- ^ The number of `clicked` messages excluding machine clicks. This metric is reliable starting April 20, 2025. [Learn more](/journeys/analytics/#delivery-metrics).
  , messageMetricsPrefetchUnderscoreclicked :: Maybe Int -- ^ The number of `clicked` messages attributed to machines. This metric is reliable starting April 20, 2025.
  , messageMetricsConverted :: Maybe Int -- ^ The timestamp when the message was `converted`.
  , messageMetricsCreated :: Maybe Int -- ^ The timestamp when the message was `created`.
  , messageMetricsDelivered :: Maybe Int -- ^ The timestamp when the message was `delivered`.
  , messageMetricsDrafted :: Maybe Int -- ^ The timestamp when the message was `drafted`.
  , messageMetricsDropped :: Maybe Int -- ^ The timestamp when the message was `dropped`.
  , messageMetricsFailed :: Maybe Int -- ^ The timestamp when the message `failed`.
  , messageMetricsOpened :: Maybe Int -- ^ The timestamp when the message was `opened`.
  , messageMetricsHumanUnderscoreopened :: Maybe Int -- ^ The number of `opened` messages excluding machine opens. This metric is reliable starting March 20, 2025. [Learn more](/journeys/analytics/#delivery-metrics).
  , messageMetricsPrefetchUnderscoreopened :: Maybe Int -- ^ The number of `opened` messages attributed to machines. This metric is reliable starting March 20, 2025.
  , messageMetricsSent :: Maybe Int -- ^ The timestamp when the message was `sent`.
  , messageMetricsSpammed :: Maybe Int -- ^ The timestamp when the message was marked as spam.
  , messageMetricsUndeliverable :: Maybe Int -- ^ The timestamp when the message became `undeliverable`.
  , messageMetricsUnsubscribed :: Maybe Int -- ^ The timestamp when a person unsubscribed based on this message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MessageMetrics where
  parseJSON = genericParseJSON optionsMessageMetrics
instance ToJSON MessageMetrics where
  toJSON = genericToJSON optionsMessageMetrics

optionsMessageMetrics :: Options
optionsMessageMetrics =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("messageMetricsBounced", "bounced")
      , ("messageMetricsClicked", "clicked")
      , ("messageMetricsHumanUnderscoreclicked", "human_clicked")
      , ("messageMetricsPrefetchUnderscoreclicked", "prefetch_clicked")
      , ("messageMetricsConverted", "converted")
      , ("messageMetricsCreated", "created")
      , ("messageMetricsDelivered", "delivered")
      , ("messageMetricsDrafted", "drafted")
      , ("messageMetricsDropped", "dropped")
      , ("messageMetricsFailed", "failed")
      , ("messageMetricsOpened", "opened")
      , ("messageMetricsHumanUnderscoreopened", "human_opened")
      , ("messageMetricsPrefetchUnderscoreopened", "prefetch_opened")
      , ("messageMetricsSent", "sent")
      , ("messageMetricsSpammed", "spammed")
      , ("messageMetricsUndeliverable", "undeliverable")
      , ("messageMetricsUnsubscribed", "unsubscribed")
      ]


-- | Describes an individual message delivery. The object contains keys for all possible parents of the message (&#x60;newsletter_id&#x60;, &#x60;broadcast_id&#x60;, etc) but only the parents of the delivery are populated. Other parent IDs are null.
data MessageObject = MessageObject
  { messageObjectId :: Maybe Text -- ^ The identifier for a delivery—the instance of a message intended for an individual recipient.
  , messageObjectDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , messageObjectMessageUnderscoretemplateUnderscoreid :: Maybe Int -- ^ The identifier of the message template used to create a message.
  , messageObjectCustomerUnderscoreid :: Maybe Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI. If your workspace supports multiple identifiers (email and ID), this value can be null.
  , messageObjectCustomerUnderscoreidentifiers :: Maybe ActivityObjectCustomerIdentifiers -- ^ 
  , messageObjectRecipient :: Maybe Text -- ^ The recipient address for an action.
  , messageObjectSubject :: Maybe Text -- ^ The subject line for an `email` action.
  , messageObjectMetrics :: Maybe MessageObjectMetrics -- ^ 
  , messageObjectCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , messageObjectFailureUnderscoremessage :: Maybe Text -- ^ Explains why a message failed, if applicable.
  , messageObjectCampaignUnderscoreid :: Maybe Int -- ^ The identifier for a campaign.
  , messageObjectActionUnderscoreid :: Maybe Int -- ^ The identifier for an action.
  , messageObjectParentUnderscoreactionUnderscoreid :: Maybe Int -- ^ The ID of the parent action, if the action occurred within a campaign and has a parent (like a randomized split, etc).
  , messageObjectNewsletterUnderscoreid :: Maybe Int -- ^ The identifier for a newsletter.
  , messageObjectContentUnderscoreid :: Maybe Int -- ^ The identifier for a message in a newsletter. Newsletters can have multiple content IDs (for multi-language messages or A/B tests).
  , messageObjectBroadcastUnderscoreid :: Maybe Int -- ^ The identifier for a broadcast.
  , messageObjectTriggerUnderscoreeventUnderscoreid :: Maybe Text -- ^ The ID of an event that triggered a campaign or action.
  , messageObjectType :: Maybe Text -- ^ The type of message/action.
  , messageObjectForgotten :: Maybe Bool -- ^ If true message contents are not retained by Customer.io.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MessageObject where
  parseJSON = genericParseJSON optionsMessageObject
instance ToJSON MessageObject where
  toJSON = genericToJSON optionsMessageObject

optionsMessageObject :: Options
optionsMessageObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("messageObjectId", "id")
      , ("messageObjectDeduplicateUnderscoreid", "deduplicate_id")
      , ("messageObjectMessageUnderscoretemplateUnderscoreid", "message_template_id")
      , ("messageObjectCustomerUnderscoreid", "customer_id")
      , ("messageObjectCustomerUnderscoreidentifiers", "customer_identifiers")
      , ("messageObjectRecipient", "recipient")
      , ("messageObjectSubject", "subject")
      , ("messageObjectMetrics", "metrics")
      , ("messageObjectCreated", "created")
      , ("messageObjectFailureUnderscoremessage", "failure_message")
      , ("messageObjectCampaignUnderscoreid", "campaign_id")
      , ("messageObjectActionUnderscoreid", "action_id")
      , ("messageObjectParentUnderscoreactionUnderscoreid", "parent_action_id")
      , ("messageObjectNewsletterUnderscoreid", "newsletter_id")
      , ("messageObjectContentUnderscoreid", "content_id")
      , ("messageObjectBroadcastUnderscoreid", "broadcast_id")
      , ("messageObjectTriggerUnderscoreeventUnderscoreid", "trigger_event_id")
      , ("messageObjectType", "type")
      , ("messageObjectForgotten", "forgotten")
      ]


-- | Metrics for an individual instance of a message; each item in the object represents the timestamp when a message achieved a particular metric. This object only contains metrics that have been recorded.
data MessageObjectMetrics = MessageObjectMetrics
  { messageObjectMetricsBounced :: Maybe Int -- ^ The timestamp when the message `bounced`.
  , messageObjectMetricsClicked :: Maybe Int -- ^ The timestamp when the message was `clicked`.
  , messageObjectMetricsHumanUnderscoreclicked :: Maybe Int -- ^ The number of `clicked` messages excluding machine clicks. This metric is reliable starting April 20, 2025. [Learn more](/journeys/analytics/#delivery-metrics).
  , messageObjectMetricsPrefetchUnderscoreclicked :: Maybe Int -- ^ The number of `clicked` messages attributed to machines. This metric is reliable starting April 20, 2025.
  , messageObjectMetricsConverted :: Maybe Int -- ^ The timestamp when the message was `converted`.
  , messageObjectMetricsCreated :: Maybe Int -- ^ The timestamp when the message was `created`.
  , messageObjectMetricsDelivered :: Maybe Int -- ^ The timestamp when the message was `delivered`.
  , messageObjectMetricsDrafted :: Maybe Int -- ^ The timestamp when the message was `drafted`.
  , messageObjectMetricsDropped :: Maybe Int -- ^ The timestamp when the message was `dropped`.
  , messageObjectMetricsFailed :: Maybe Int -- ^ The timestamp when the message `failed`.
  , messageObjectMetricsOpened :: Maybe Int -- ^ The timestamp when the message was `opened`.
  , messageObjectMetricsHumanUnderscoreopened :: Maybe Int -- ^ The number of `opened` messages excluding machine opens. This metric is reliable starting March 20, 2025. [Learn more](/journeys/analytics/#delivery-metrics).
  , messageObjectMetricsPrefetchUnderscoreopened :: Maybe Int -- ^ The number of `opened` messages attributed to machines. This metric is reliable starting March 20, 2025.
  , messageObjectMetricsSent :: Maybe Int -- ^ The timestamp when the message was `sent`.
  , messageObjectMetricsSpammed :: Maybe Int -- ^ The timestamp when the message was marked as spam.
  , messageObjectMetricsUndeliverable :: Maybe Int -- ^ The timestamp when the message became `undeliverable`.
  , messageObjectMetricsUnsubscribed :: Maybe Int -- ^ The timestamp when a person unsubscribed based on this message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MessageObjectMetrics where
  parseJSON = genericParseJSON optionsMessageObjectMetrics
instance ToJSON MessageObjectMetrics where
  toJSON = genericToJSON optionsMessageObjectMetrics

optionsMessageObjectMetrics :: Options
optionsMessageObjectMetrics =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("messageObjectMetricsBounced", "bounced")
      , ("messageObjectMetricsClicked", "clicked")
      , ("messageObjectMetricsHumanUnderscoreclicked", "human_clicked")
      , ("messageObjectMetricsPrefetchUnderscoreclicked", "prefetch_clicked")
      , ("messageObjectMetricsConverted", "converted")
      , ("messageObjectMetricsCreated", "created")
      , ("messageObjectMetricsDelivered", "delivered")
      , ("messageObjectMetricsDrafted", "drafted")
      , ("messageObjectMetricsDropped", "dropped")
      , ("messageObjectMetricsFailed", "failed")
      , ("messageObjectMetricsOpened", "opened")
      , ("messageObjectMetricsHumanUnderscoreopened", "human_opened")
      , ("messageObjectMetricsPrefetchUnderscoreopened", "prefetch_opened")
      , ("messageObjectMetricsSent", "sent")
      , ("messageObjectMetricsSpammed", "spammed")
      , ("messageObjectMetricsUndeliverable", "undeliverable")
      , ("messageObjectMetricsUnsubscribed", "unsubscribed")
      ]


-- | Total metrics over the lifespan of the requested item.
data MessageTotalMetrics = MessageTotalMetrics
  { messageTotalMetricsBounced :: Maybe Int -- ^ The number of `bounced` messages.
  , messageTotalMetricsClicked :: Maybe Int -- ^ The number of `clicked` messages.
  , messageTotalMetricsConverted :: Maybe Int -- ^ The number of `converted` messages.
  , messageTotalMetricsCreated :: Maybe Int -- ^ The number of `created` messages.
  , messageTotalMetricsDelivered :: Maybe Int -- ^ The number of `delivered` messages.
  , messageTotalMetricsDrafted :: Maybe Int -- ^ The number of `drafted` messages.
  , messageTotalMetricsDropped :: Maybe Int -- ^ The number of `dropped` messages.
  , messageTotalMetricsFailed :: Maybe Int -- ^ The number of `failed` messages.
  , messageTotalMetricsOpened :: Maybe Int -- ^ The number of `opened` messages
  , messageTotalMetricsSent :: Maybe Int -- ^ The number of `sent` messages
  , messageTotalMetricsSpammed :: Maybe Int -- ^ The number of messages marked as spam.
  , messageTotalMetricsUndeliverable :: Maybe Int -- ^ The number of `undeliverable` messages.
  , messageTotalMetricsUnsubscribed :: Maybe Int -- ^ The number of unsubscribes attributed to messages.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MessageTotalMetrics where
  parseJSON = genericParseJSON optionsMessageTotalMetrics
instance ToJSON MessageTotalMetrics where
  toJSON = genericToJSON optionsMessageTotalMetrics

optionsMessageTotalMetrics :: Options
optionsMessageTotalMetrics =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("messageTotalMetricsBounced", "bounced")
      , ("messageTotalMetricsClicked", "clicked")
      , ("messageTotalMetricsConverted", "converted")
      , ("messageTotalMetricsCreated", "created")
      , ("messageTotalMetricsDelivered", "delivered")
      , ("messageTotalMetricsDrafted", "drafted")
      , ("messageTotalMetricsDropped", "dropped")
      , ("messageTotalMetricsFailed", "failed")
      , ("messageTotalMetricsOpened", "opened")
      , ("messageTotalMetricsSent", "sent")
      , ("messageTotalMetricsSpammed", "spammed")
      , ("messageTotalMetricsUndeliverable", "undeliverable")
      , ("messageTotalMetricsUnsubscribed", "unsubscribed")
      ]


-- | The type of message/action.
data MessageType = MessageType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MessageType where
  parseJSON = genericParseJSON optionsMessageType
instance ToJSON MessageType where
  toJSON = genericToJSON optionsMessageType

optionsMessageType :: Options
optionsMessageType =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | Determines the metric(s) you want to return.
data Metric = Metric
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Metric where
  parseJSON = genericParseJSON optionsMetric
instance ToJSON Metric where
  toJSON = genericToJSON optionsMetric

optionsMetric :: Options
optionsMetric =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data MetricsRequest = MetricsRequest
  { metricsRequestDeliveryUnderscoreid :: Text -- ^ The CIO-Delivery-ID from the notification that you want to associate the `event` with.
  , metricsRequestTimestamp :: Maybe Int -- ^ The unix timestamp when the event occurred.
  , metricsRequestMetric :: Text -- ^ The type of device-side event you want to report back to Customer.io.
  , metricsRequestRecipient :: Maybe Int -- ^ The phone number of the person who received the message.
  , metricsRequestReason :: Maybe Text -- ^ For metrics indicating a failure (like `bounced`), this field provides the reason for the failure.
  , metricsRequestHref :: Maybe Text -- ^ For `clicked` metrics, this is the link the recipient clicked.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MetricsRequest where
  parseJSON = genericParseJSON optionsMetricsRequest
instance ToJSON MetricsRequest where
  toJSON = genericToJSON optionsMetricsRequest

optionsMetricsRequest :: Options
optionsMetricsRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("metricsRequestDeliveryUnderscoreid", "delivery_id")
      , ("metricsRequestTimestamp", "timestamp")
      , ("metricsRequestMetric", "metric")
      , ("metricsRequestRecipient", "recipient")
      , ("metricsRequestReason", "reason")
      , ("metricsRequestHref", "href")
      ]


-- | 
data MobileScreenView = MobileScreenView
  { mobileScreenViewAnonymousUnderscoreid :: Text -- ^ An identifier for an anonymous event, like a cookie. If set as an attribute on a person, any events bearing the same anonymous value are associated with this person. This value must be unique and is not reusable.
  , mobileScreenViewName :: Text -- ^ The name of the event. In general, this should be the name of the screen or deep link path that a person viewed, making it easy to segment your audience or trigger campaigns from these events. Make sure you trim leading and trailing spaces from this field.
  , mobileScreenViewId :: Maybe Text -- ^ An identifier used to deduplicate events. This value must be a [ULID](https://github.com/ulid/spec). If an event has the same value as an event we previously received, we won't show or process the duplicate. Note - our Python and Ruby libraries do not pass this id.
  , mobileScreenViewType :: Text -- ^ Indicates that the event represents a mobile screen view. You can also capture screen events directly with [our iOS SDK](/sdk/ios/track-events/#screen-view-events).
  , mobileScreenViewTimestamp :: Maybe Int -- ^ The unix timestamp when the event took place. If you don't provide this value, we use the date-time when we receive the event. 
  , mobileScreenViewData :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on your customer (referenced by `customer_id`).
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MobileScreenView where
  parseJSON = genericParseJSON optionsMobileScreenView
instance ToJSON MobileScreenView where
  toJSON = genericToJSON optionsMobileScreenView

optionsMobileScreenView :: Options
optionsMobileScreenView =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("mobileScreenViewAnonymousUnderscoreid", "anonymous_id")
      , ("mobileScreenViewName", "name")
      , ("mobileScreenViewId", "id")
      , ("mobileScreenViewType", "type")
      , ("mobileScreenViewTimestamp", "timestamp")
      , ("mobileScreenViewData", "data")
      ]


-- | 
data MobileScreenView1 = MobileScreenView1
  { mobileScreenView1AnonymousUnderscoreid :: Maybe Text -- ^ An identifier for an anonymous event, like a cookie. If set as an attribute on a person, any events bearing the same anonymous value are associated with this person. This value must be unique and is not reusable.
  , mobileScreenView1Name :: Text -- ^ The name of the event. In general, this should be the name of the screen or deep link path that a person viewed, making it easy to segment your audience or trigger campaigns using this event. Make sure you trim leading and trailing spaces from this field.
  , mobileScreenView1Id :: Maybe Text -- ^ An identifier used to deduplicate events. This value must be a [ULID](https://github.com/ulid/spec). If an event has the same value as an event we previously received, we won't show or process the duplicate. Note - our Python and Ruby libraries do not pass this id.
  , mobileScreenView1Type :: Text -- ^ Indicates that the event represents a mobile screen view. You can also capture screen events directly with [our iOS SDK](/sdk/ios/track-events/#screen-view-events).
  , mobileScreenView1Timestamp :: Maybe Int -- ^ The unix timestamp when the event took place. If you don't provide this value, we use the date-time when we receive the event. 
  , mobileScreenView1Data :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on your customer (referenced by `customer_id`).
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MobileScreenView1 where
  parseJSON = genericParseJSON optionsMobileScreenView1
instance ToJSON MobileScreenView1 where
  toJSON = genericToJSON optionsMobileScreenView1

optionsMobileScreenView1 :: Options
optionsMobileScreenView1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("mobileScreenView1AnonymousUnderscoreid", "anonymous_id")
      , ("mobileScreenView1Name", "name")
      , ("mobileScreenView1Id", "id")
      , ("mobileScreenView1Type", "type")
      , ("mobileScreenView1Timestamp", "timestamp")
      , ("mobileScreenView1Data", "data")
      ]


-- | 
data MsgTemplateIdsInner = MsgTemplateIdsInner
  { msgTemplateIdsInnerId :: Maybe Int -- ^ The identifier for the template.
  , msgTemplateIdsInnerType :: Maybe Text -- ^ The type of template.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MsgTemplateIdsInner where
  parseJSON = genericParseJSON optionsMsgTemplateIdsInner
instance ToJSON MsgTemplateIdsInner where
  toJSON = genericToJSON optionsMsgTemplateIdsInner

optionsMsgTemplateIdsInner :: Options
optionsMsgTemplateIdsInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("msgTemplateIdsInnerId", "id")
      , ("msgTemplateIdsInnerType", "type")
      ]


-- | 
data NewsletterObject = NewsletterObject
  { newsletterObjectId :: Maybe Int -- ^ The identifier for a newsletter.
  , newsletterObjectDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , newsletterObjectContentUnderscoreids :: Maybe [Int] -- ^ A list of messages contained by a newsletter. If your newsletter includes a list of languages and/or A/B test variants, the array contains a content ID for each language/variant. [Look up a newsletter variant](#operation/getNewsletterVariant) to get more information about an individual content ID.
  , newsletterObjectName :: Maybe Text -- ^ The name of the newsletter.
  , newsletterObjectSentUnderscoreat :: Maybe Int -- ^ The last time the newsletter was sent.
  , newsletterObjectCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , newsletterObjectUpdated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , newsletterObjectType :: Maybe Text -- ^ The type of newsletter—`email`, `twilio`, etc.
  , newsletterObjectTags :: Maybe [Text] -- ^ An array of tags you set on this newsletter.
  , newsletterObjectRecipientUnderscoresegmentUnderscoreids :: Maybe [Int] -- ^ A list of segments used in a newsletter's recipient filter, returned if newsletter recipients were filtered by one or more segments.
  , newsletterObjectSubscriptionUnderscoretopicUnderscoreid :: Maybe Int -- ^ The identifier of a subscription topic. Remember, subscription topics are assigned an incrementing number, starting at 1.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON NewsletterObject where
  parseJSON = genericParseJSON optionsNewsletterObject
instance ToJSON NewsletterObject where
  toJSON = genericToJSON optionsNewsletterObject

optionsNewsletterObject :: Options
optionsNewsletterObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("newsletterObjectId", "id")
      , ("newsletterObjectDeduplicateUnderscoreid", "deduplicate_id")
      , ("newsletterObjectContentUnderscoreids", "content_ids")
      , ("newsletterObjectName", "name")
      , ("newsletterObjectSentUnderscoreat", "sent_at")
      , ("newsletterObjectCreated", "created")
      , ("newsletterObjectUpdated", "updated")
      , ("newsletterObjectType", "type")
      , ("newsletterObjectTags", "tags")
      , ("newsletterObjectRecipientUnderscoresegmentUnderscoreids", "recipient_segment_ids")
      , ("newsletterObjectSubscriptionUnderscoretopicUnderscoreid", "subscription_topic_id")
      ]


-- | Returns results if a condition is false. While and/or support an array of items, &#x60;not&#x60; supports a single filter object.
data Not = Not
  { notAnd :: Maybe [PeopleFilter] -- ^ Match *all* conditions to return results.
  , notOr :: Maybe [PeopleFilter] -- ^ Match *any* condition to return results.
  , notSegment :: Maybe Segment -- ^ 
  , notAttribute :: Maybe Attribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Not where
  parseJSON = genericParseJSON optionsNot
instance ToJSON Not where
  toJSON = genericToJSON optionsNot

optionsNot :: Options
optionsNot =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("notAnd", "and")
      , ("notOr", "or")
      , ("notSegment", "segment")
      , ("notAttribute", "attribute")
      ]


-- | 
data Not1 = Not1
  { not1Not :: Maybe ObjectFilterNotNot -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Not1 where
  parseJSON = genericParseJSON optionsNot1
instance ToJSON Not1 where
  toJSON = genericToJSON optionsNot1

optionsNot1 :: Options
optionsNot1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("not1Not", "not")
      ]


-- | Returns results if a condition is false. While and/or support an array of items, &#x60;not&#x60; supports a single filter object.
data NotAudienceFilter = NotAudienceFilter
  { notAudienceFilterAnd :: Maybe [PeopleFilter] -- ^ Match *all* conditions to return results.
  , notAudienceFilterOr :: Maybe [PeopleFilter] -- ^ Match *any* condition to return results.
  , notAudienceFilterSegment :: Maybe Segment -- ^ 
  , notAudienceFilterAttribute :: Maybe Attribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON NotAudienceFilter where
  parseJSON = genericParseJSON optionsNotAudienceFilter
instance ToJSON NotAudienceFilter where
  toJSON = genericToJSON optionsNotAudienceFilter

optionsNotAudienceFilter :: Options
optionsNotAudienceFilter =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("notAudienceFilterAnd", "and")
      , ("notAudienceFilterOr", "or")
      , ("notAudienceFilterSegment", "segment")
      , ("notAudienceFilterAttribute", "attribute")
      ]


-- | 
data NotificationAndDataObject = NotificationAndDataObject
  { notificationAndDataObjectNotification :: NotificationAndDataObjectNotification -- ^ 
  , notificationAndDataObjectData :: Maybe FcmBasicPushMessageData -- ^ 
  , notificationAndDataObjectAndroid :: Maybe DataObjectOnlyAndroid -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON NotificationAndDataObject where
  parseJSON = genericParseJSON optionsNotificationAndDataObject
instance ToJSON NotificationAndDataObject where
  toJSON = genericToJSON optionsNotificationAndDataObject

optionsNotificationAndDataObject :: Options
optionsNotificationAndDataObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("notificationAndDataObjectNotification", "notification")
      , ("notificationAndDataObjectData", "data")
      , ("notificationAndDataObjectAndroid", "android")
      ]


-- | Contains properties interpreted by the SDK except for the &#x60;link&#x60;.
data NotificationAndDataObjectNotification = NotificationAndDataObjectNotification
  { notificationAndDataObjectNotificationTitle :: Maybe Text -- ^ The title of your push notification.
  , notificationAndDataObjectNotificationBody :: Maybe Text -- ^ The body of your push notification.
  , notificationAndDataObjectNotificationImage :: Maybe Text -- ^ The URL of an HTTPS image that you want to use for your message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON NotificationAndDataObjectNotification where
  parseJSON = genericParseJSON optionsNotificationAndDataObjectNotification
instance ToJSON NotificationAndDataObjectNotification where
  toJSON = genericToJSON optionsNotificationAndDataObjectNotification

optionsNotificationAndDataObjectNotification :: Options
optionsNotificationAndDataObjectNotification =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("notificationAndDataObjectNotificationTitle", "title")
      , ("notificationAndDataObjectNotificationBody", "body")
      , ("notificationAndDataObjectNotificationImage", "image")
      ]


-- | Contains your import parameters.
data Object = Object
  { objectName :: Text -- ^ A friendly name for your import. This helps you identify your import.
  , objectDataUnderscorefileUnderscoreurl :: Text -- ^ The URL or path to the CSV file you want to import.
  , objectObjectUnderscoretypeUnderscoreid :: Text -- ^ The object type an object belongs to—like \"Companies\" or \"Accounts\". Object type IDs are string-formatted integers that begin at `1` and increment for each new type.
  , objectType :: Text -- ^ The type of import.
  , objectDataUnderscoretoUnderscoreprocess :: Maybe Text -- ^ Determines whether your import operation performs `all` add/update operations, only adds items (`only_new`), or only updates existing items (`only_existing`). Defaults to `all`. If `import_type` is `event`, you can only use `all` or `only_existing`.   This field was previously called `people_to_process` - we still support it but will deprecate it soon. 
  , objectDescription :: Maybe Text -- ^ A helpful description that can help you find and recognize your import operation.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Object where
  parseJSON = genericParseJSON optionsObject
instance ToJSON Object where
  toJSON = genericToJSON optionsObject

optionsObject :: Options
optionsObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectName", "name")
      , ("objectDataUnderscorefileUnderscoreurl", "data_file_url")
      , ("objectObjectUnderscoretypeUnderscoreid", "object_type_id")
      , ("objectType", "type")
      , ("objectDataUnderscoretoUnderscoreprocess", "data_to_process")
      , ("objectDescription", "description")
      ]


-- | 
data Object1 = Object1
  { object1Identifiers :: IdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers -- ^ 
  , object1Type :: Text -- ^ The operation modifies a single object—non person data.
  , object1Action :: Text -- ^ This operation deletes an object relationship from one or more people.
  , object1Attributes :: Maybe Value -- ^ The data that belongs to the object. This is information you might want to associate with people later (through `cio_relationships`). Passing `null` or an empty string removes the attribute from the object. Some attributes have special meaning. Please refer to the list of [reserved attributes](/journeys/objects-create/#reserved-attributes). 
  , object1CioUnderscorerelationships :: [Identify1AllOfCioRelationshipsInner] -- ^ The people you want to associate with an object. Each object in the array represents a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Object1 where
  parseJSON = genericParseJSON optionsObject1
instance ToJSON Object1 where
  toJSON = genericToJSON optionsObject1

optionsObject1 :: Options
optionsObject1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("object1Identifiers", "identifiers")
      , ("object1Type", "type")
      , ("object1Action", "action")
      , ("object1Attributes", "attributes")
      , ("object1CioUnderscorerelationships", "cio_relationships")
      ]


-- | 
data Object2 = Object2
  { object2Id :: Maybe Int -- ^ The identifier for a campaign.
  , object2DeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , object2Name :: Maybe Text -- ^ The name of the campaign.
  , object2Type :: Maybe Text -- ^ The type of campaign trigger. **Sunsetting on March 30, 2025**
  , object2Created :: Maybe Int -- ^ The date time when the referenced ID was created.
  , object2Updated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , object2Active :: Maybe Bool -- ^ If true, the campaign is active and can still send messages.
  , object2State :: Maybe Text -- ^ The status of the campaign.
  , object2Actions :: Maybe [SegmentActionsInner] -- ^ An array of actions contained within the campaign.
  , object2FirstUnderscorestarted :: Maybe Int -- ^ The date and time when you first started the campaign and it first became eligible to be triggered.
  , object2Tags :: Maybe [Text] -- ^ An array of tags you set on this campaign.
  , object2FilterUnderscoresegmentUnderscoreids :: Maybe [Int] -- ^ A list of segments used in the campaign filter, returned if the campaign audience was filtered on one or more segments.
  , object2ObjectUnderscoretypeUnderscoreid :: Maybe Int -- ^ The the object type ID of the trigger.
  , object2FilterUnderscoreobjectUnderscoreattributes :: Maybe Text -- ^ A list of object attributes used in the campaign filter, returned if the campaign audience was filtered on one or more object attributes.
  , object2FilterUnderscorerelationshipUnderscoreattributes :: Maybe Text -- ^ A list of relationship attributes used in the campaign filter, returned if the campaign audience was filtered on one or more relationship attributes.
  , object2Audience :: Maybe Object2Audience -- ^ 
  , object2ObjectUnderscoreattributeUnderscoretriggers :: Maybe Value -- ^ A list of object attributes used to trigger the campaign.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Object2 where
  parseJSON = genericParseJSON optionsObject2
instance ToJSON Object2 where
  toJSON = genericToJSON optionsObject2

optionsObject2 :: Options
optionsObject2 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("object2Id", "id")
      , ("object2DeduplicateUnderscoreid", "deduplicate_id")
      , ("object2Name", "name")
      , ("object2Type", "type")
      , ("object2Created", "created")
      , ("object2Updated", "updated")
      , ("object2Active", "active")
      , ("object2State", "state")
      , ("object2Actions", "actions")
      , ("object2FirstUnderscorestarted", "first_started")
      , ("object2Tags", "tags")
      , ("object2FilterUnderscoresegmentUnderscoreids", "filter_segment_ids")
      , ("object2ObjectUnderscoretypeUnderscoreid", "object_type_id")
      , ("object2FilterUnderscoreobjectUnderscoreattributes", "filter_object_attributes")
      , ("object2FilterUnderscorerelationshipUnderscoreattributes", "filter_relationship_attributes")
      , ("object2Audience", "audience")
      , ("object2ObjectUnderscoreattributeUnderscoretriggers", "object_attribute_triggers")
      ]


-- | Defines the people who will start a journey in your campaign.
data Object2Audience = Object2Audience
  { object2AudienceType :: Maybe Int -- ^ The type of audience selected. \"Every person in the object\" is `1`. \"Certain people in the object\" is also `1`. \"Certain people\" will always have one or more audience filters (see below). \"Every person\" will never have an audience filter.
  , object2AudiencePersonUnderscorefilters :: Maybe Value -- ^ Returns the profile attributes you filtered the audience by, if any. Only applies to `type 1`, \"Certain people in the object\".
  , object2AudienceRelationshipUnderscorefilters :: Maybe Value -- ^ Returns the relationship attributes you filtered the audience by, if any. Only applies to `type 1`, \"Certain people in the object\".
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Object2Audience where
  parseJSON = genericParseJSON optionsObject2Audience
instance ToJSON Object2Audience where
  toJSON = genericToJSON optionsObject2Audience

optionsObject2Audience :: Options
optionsObject2Audience =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("object2AudienceType", "type")
      , ("object2AudiencePersonUnderscorefilters", "person_filters")
      , ("object2AudienceRelationshipUnderscorefilters", "relationship_filters")
      ]


-- | Add relationships between an object and one or more people.
data ObjectAddRelationships = ObjectAddRelationships
  { objectAddRelationshipsIdentifiers :: Maybe OneOf&lt;object,object&gt; -- ^ The identifiers for a particular object. You can use either the `object_type_id` and `object_id` (where `object_type_id` represents the type of object and the `object_id` is the individual identifier for the object) or the `cio_object_id`.
  , objectAddRelationshipsType :: Text -- ^ The operation modifies a single object—non person data.
  , objectAddRelationshipsAction :: Text -- ^ This operation associates an object with one or more people.
  , objectAddRelationshipsCioUnderscorerelationships :: [ObjectAddRelationshipsAllOfCioRelationships] -- ^ The people you want to associate with an object. Each object in the array represents a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectAddRelationships where
  parseJSON = genericParseJSON optionsObjectAddRelationships
instance ToJSON ObjectAddRelationships where
  toJSON = genericToJSON optionsObjectAddRelationships

optionsObjectAddRelationships :: Options
optionsObjectAddRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectAddRelationshipsIdentifiers", "identifiers")
      , ("objectAddRelationshipsType", "type")
      , ("objectAddRelationshipsAction", "action")
      , ("objectAddRelationshipsCioUnderscorerelationships", "cio_relationships")
      ]


-- | 
data ObjectAddRelationshipsAllOfCioRelationships = ObjectAddRelationshipsAllOfCioRelationships
  { objectAddRelationshipsAllOfCioRelationshipsIdentifiers :: Maybe ObjectAddRelationshipsAllOfIdentifiers -- ^ 
  , objectAddRelationshipsAllOfCioRelationshipsRelationshipUnderscoreattributes :: Maybe (Map.Map String Value) -- ^ The attributes associated with a relationship. Passing null or an empty string removes the attribute from the relationship. 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectAddRelationshipsAllOfCioRelationships where
  parseJSON = genericParseJSON optionsObjectAddRelationshipsAllOfCioRelationships
instance ToJSON ObjectAddRelationshipsAllOfCioRelationships where
  toJSON = genericToJSON optionsObjectAddRelationshipsAllOfCioRelationships

optionsObjectAddRelationshipsAllOfCioRelationships :: Options
optionsObjectAddRelationshipsAllOfCioRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectAddRelationshipsAllOfCioRelationshipsIdentifiers", "identifiers")
      , ("objectAddRelationshipsAllOfCioRelationshipsRelationshipUnderscoreattributes", "relationship_attributes")
      ]


-- | 
data ObjectAddRelationshipsAllOfIdentifiers = ObjectAddRelationshipsAllOfIdentifiers
  { objectAddRelationshipsAllOfIdentifiersId :: Maybe Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , objectAddRelationshipsAllOfIdentifiersEmail :: Maybe Text -- ^ The email address of the customer.
  , objectAddRelationshipsAllOfIdentifiersCioUnderscoreid :: Maybe Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectAddRelationshipsAllOfIdentifiers where
  parseJSON = genericParseJSON optionsObjectAddRelationshipsAllOfIdentifiers
instance ToJSON ObjectAddRelationshipsAllOfIdentifiers where
  toJSON = genericToJSON optionsObjectAddRelationshipsAllOfIdentifiers

optionsObjectAddRelationshipsAllOfIdentifiers :: Options
optionsObjectAddRelationshipsAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectAddRelationshipsAllOfIdentifiersId", "id")
      , ("objectAddRelationshipsAllOfIdentifiersEmail", "email")
      , ("objectAddRelationshipsAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | Filter your objects by their attributes.
data ObjectAttribute = ObjectAttribute
  { objectAttributeField :: Text -- ^ The name of the attribute you want to filter against.
  , objectAttributeOperator :: Text -- ^ Determine how to evaluate criteria against the field—`exists` returns results if an object has the attribute; `eq` returns results an object's attribute exists and the attribute has the `value` you specify.
  , objectAttributeValue :: Maybe Text -- ^ The value you want to match for this attribute. You must include a value if you use the `eq` operator.
  , objectAttributeTypeUnderscoreid :: Text -- ^ The object type an object belongs to—like \"Companies\" or \"Accounts\". Object type IDs are string-formatted integers that begin at `1` and increment for each new type.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectAttribute where
  parseJSON = genericParseJSON optionsObjectAttribute
instance ToJSON ObjectAttribute where
  toJSON = genericToJSON optionsObjectAttribute

optionsObjectAttribute :: Options
optionsObjectAttribute =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectAttributeField", "field")
      , ("objectAttributeOperator", "operator")
      , ("objectAttributeValue", "value")
      , ("objectAttributeTypeUnderscoreid", "type_id")
      ]


-- | A simple filter to find objects matching an attribute condition.
data ObjectAttribute1 = ObjectAttribute1
  { objectAttribute1ObjectUnderscoreattribute :: Maybe ObjectAttribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectAttribute1 where
  parseJSON = genericParseJSON optionsObjectAttribute1
instance ToJSON ObjectAttribute1 where
  toJSON = genericToJSON optionsObjectAttribute1

optionsObjectAttribute1 :: Options
optionsObjectAttribute1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectAttribute1ObjectUnderscoreattribute", "object_attribute")
      ]


-- | 
data ObjectCommon = ObjectCommon
  { objectCommonIdentifiers :: Maybe ObjectCommonAllOfIdentifiers -- ^ 
  , objectCommonType :: Text -- ^ The operation modifies a single object—non person data.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectCommon where
  parseJSON = genericParseJSON optionsObjectCommon
instance ToJSON ObjectCommon where
  toJSON = genericToJSON optionsObjectCommon

optionsObjectCommon :: Options
optionsObjectCommon =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectCommonIdentifiers", "identifiers")
      , ("objectCommonType", "type")
      ]


-- | The identifiers for a particular object. You can use either the &#x60;object_type_id&#x60; and &#x60;object_id&#x60; (where &#x60;object_type_id&#x60; represents the type of object and the &#x60;object_id&#x60; is the individual identifier for the object) or the &#x60;cio_object_id&#x60;.
data ObjectCommonAllOfIdentifiers = ObjectCommonAllOfIdentifiers
  { objectCommonAllOfIdentifiersObjectUnderscoretypeUnderscoreid :: Text -- ^ The object type an object belongs to—like \"Companies\" or \"Accounts\". Object type IDs are string-formatted integers that begin at `1` and increment for each new type.
  , objectCommonAllOfIdentifiersObjectUnderscoreid :: Text -- ^ The unique identifier for an object. If you use an `object_id` that already exists, we'll update the object accordingly.
  , objectCommonAllOfIdentifiersCioUnderscoreobjectUnderscoreid :: Text -- ^ A unique value that Customer.io sets for an object when you create it. This ID is immutable.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectCommonAllOfIdentifiers where
  parseJSON = genericParseJSON optionsObjectCommonAllOfIdentifiers
instance ToJSON ObjectCommonAllOfIdentifiers where
  toJSON = genericToJSON optionsObjectCommonAllOfIdentifiers

optionsObjectCommonAllOfIdentifiers :: Options
optionsObjectCommonAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectCommonAllOfIdentifiersObjectUnderscoretypeUnderscoreid", "object_type_id")
      , ("objectCommonAllOfIdentifiersObjectUnderscoreid", "object_id")
      , ("objectCommonAllOfIdentifiersCioUnderscoreobjectUnderscoreid", "cio_object_id")
      ]


-- | 
data ObjectCommonIdentify = ObjectCommonIdentify
  { objectCommonIdentifyIdentifiers :: Identify1AllOfIdentifiers -- ^ 
  , objectCommonIdentifyType :: Text -- ^ The operation modifies a single object—non person data.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectCommonIdentify where
  parseJSON = genericParseJSON optionsObjectCommonIdentify
instance ToJSON ObjectCommonIdentify where
  toJSON = genericToJSON optionsObjectCommonIdentify

optionsObjectCommonIdentify :: Options
optionsObjectCommonIdentify =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectCommonIdentifyIdentifiers", "identifiers")
      , ("objectCommonIdentifyType", "type")
      ]


-- | Delete an object. This also removes relationships from people. 
data ObjectDelete = ObjectDelete
  { objectDeleteIdentifiers :: Maybe OneOf&lt;object,object&gt; -- ^ The identifiers for a particular object. You can use either the `object_type_id` and `object_id` (where `object_type_id` represents the type of object and the `object_id` is the individual identifier for the object) or the `cio_object_id`.
  , objectDeleteType :: Text -- ^ The operation modifies a single object—non person data.
  , objectDeleteAction :: Text -- ^ Indicates that the operation will `delete` the the item of the specified `type`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectDelete where
  parseJSON = genericParseJSON optionsObjectDelete
instance ToJSON ObjectDelete where
  toJSON = genericToJSON optionsObjectDelete

optionsObjectDelete :: Options
optionsObjectDelete =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectDeleteIdentifiers", "identifiers")
      , ("objectDeleteType", "type")
      , ("objectDeleteAction", "action")
      ]


-- | Delete relationships between an object and one or more people.
data ObjectDeleteRelationships = ObjectDeleteRelationships
  { objectDeleteRelationshipsIdentifiers :: Maybe OneOf&lt;object,object&gt; -- ^ The identifiers for a particular object. You can use either the `object_type_id` and `object_id` (where `object_type_id` represents the type of object and the `object_id` is the individual identifier for the object) or the `cio_object_id`.
  , objectDeleteRelationshipsType :: Text -- ^ The operation modifies a single object—non person data.
  , objectDeleteRelationshipsAction :: Text -- ^ This operation deletes an object relationship from one or more people.
  , objectDeleteRelationshipsCioUnderscorerelationships :: [ObjectDeleteRelationshipsAllOfCioRelationships] -- ^ The people you want to associate with an object. Each object in the array represents a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectDeleteRelationships where
  parseJSON = genericParseJSON optionsObjectDeleteRelationships
instance ToJSON ObjectDeleteRelationships where
  toJSON = genericToJSON optionsObjectDeleteRelationships

optionsObjectDeleteRelationships :: Options
optionsObjectDeleteRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectDeleteRelationshipsIdentifiers", "identifiers")
      , ("objectDeleteRelationshipsType", "type")
      , ("objectDeleteRelationshipsAction", "action")
      , ("objectDeleteRelationshipsCioUnderscorerelationships", "cio_relationships")
      ]


-- | 
data ObjectDeleteRelationshipsAllOfCioRelationships = ObjectDeleteRelationshipsAllOfCioRelationships
  { objectDeleteRelationshipsAllOfCioRelationshipsIdentifiers :: Maybe ObjectDeleteRelationshipsAllOfIdentifiers -- ^ 
  , objectDeleteRelationshipsAllOfCioRelationshipsRelationshipUnderscoreattributes :: Maybe (Map.Map String Value) -- ^ The attributes associated with a relationship. Passing null or an empty string removes the attribute from the relationship. 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectDeleteRelationshipsAllOfCioRelationships where
  parseJSON = genericParseJSON optionsObjectDeleteRelationshipsAllOfCioRelationships
instance ToJSON ObjectDeleteRelationshipsAllOfCioRelationships where
  toJSON = genericToJSON optionsObjectDeleteRelationshipsAllOfCioRelationships

optionsObjectDeleteRelationshipsAllOfCioRelationships :: Options
optionsObjectDeleteRelationshipsAllOfCioRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectDeleteRelationshipsAllOfCioRelationshipsIdentifiers", "identifiers")
      , ("objectDeleteRelationshipsAllOfCioRelationshipsRelationshipUnderscoreattributes", "relationship_attributes")
      ]


-- | 
data ObjectDeleteRelationshipsAllOfIdentifiers = ObjectDeleteRelationshipsAllOfIdentifiers
  { objectDeleteRelationshipsAllOfIdentifiersId :: Maybe Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , objectDeleteRelationshipsAllOfIdentifiersEmail :: Maybe Text -- ^ The email address of the customer.
  , objectDeleteRelationshipsAllOfIdentifiersCioUnderscoreid :: Maybe Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectDeleteRelationshipsAllOfIdentifiers where
  parseJSON = genericParseJSON optionsObjectDeleteRelationshipsAllOfIdentifiers
instance ToJSON ObjectDeleteRelationshipsAllOfIdentifiers where
  toJSON = genericToJSON optionsObjectDeleteRelationshipsAllOfIdentifiers

optionsObjectDeleteRelationshipsAllOfIdentifiers :: Options
optionsObjectDeleteRelationshipsAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectDeleteRelationshipsAllOfIdentifiersId", "id")
      , ("objectDeleteRelationshipsAllOfIdentifiersEmail", "email")
      , ("objectDeleteRelationshipsAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | When filtering for objects, you can use &#x60;and&#x60; and &#x60;or&#x60; arrays to determine the logic for a group of filter conditions. &#x60;not&#x60; reverses the filter condition and matches when the condition is false. &#x60;object_attribute&#x60; represents the individual conditions you can filter objects by.  The top level of this object can only contain a single property, but you can nest &#x60;and&#x60; and &#x60;or&#x60; properties to produce complex filters. 
data ObjectFilter = ObjectFilter
  { objectFilterAnd :: Maybe [ObjectFilterAndAndInner] -- ^ Match *all* conditions to return results.
  , objectFilterOr :: Maybe [ObjectFilterAndAndInner] -- ^ Match *any* condition to return results.
  , objectFilterNot :: Maybe ObjectFilterNotNot -- ^ 
  , objectFilterObjectUnderscoreattribute :: Maybe ObjectAttribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectFilter where
  parseJSON = genericParseJSON optionsObjectFilter
instance ToJSON ObjectFilter where
  toJSON = genericToJSON optionsObjectFilter

optionsObjectFilter :: Options
optionsObjectFilter =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectFilterAnd", "and")
      , ("objectFilterOr", "or")
      , ("objectFilterNot", "not")
      , ("objectFilterObjectUnderscoreattribute", "object_attribute")
      ]


-- | 
data ObjectFilterAnd = ObjectFilterAnd
  { objectFilterAndAnd :: Maybe [ObjectFilterAndAndInner] -- ^ Match *all* conditions to return results.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectFilterAnd where
  parseJSON = genericParseJSON optionsObjectFilterAnd
instance ToJSON ObjectFilterAnd where
  toJSON = genericToJSON optionsObjectFilterAnd

optionsObjectFilterAnd :: Options
optionsObjectFilterAnd =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectFilterAndAnd", "and")
      ]


-- | 
data ObjectFilterAndAndInner = ObjectFilterAndAndInner
  { objectFilterAndAndInnerObjectUnderscoreattribute :: Maybe ObjectAttribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectFilterAndAndInner where
  parseJSON = genericParseJSON optionsObjectFilterAndAndInner
instance ToJSON ObjectFilterAndAndInner where
  toJSON = genericToJSON optionsObjectFilterAndAndInner

optionsObjectFilterAndAndInner :: Options
optionsObjectFilterAndAndInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectFilterAndAndInnerObjectUnderscoreattribute", "object_attribute")
      ]


-- | 
data ObjectFilterNot = ObjectFilterNot
  { objectFilterNotNot :: Maybe ObjectFilterNotNot -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectFilterNot where
  parseJSON = genericParseJSON optionsObjectFilterNot
instance ToJSON ObjectFilterNot where
  toJSON = genericToJSON optionsObjectFilterNot

optionsObjectFilterNot :: Options
optionsObjectFilterNot =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectFilterNotNot", "not")
      ]


-- | Returns results if a condition is false. While and/or support an array of items, &#x60;not&#x60; supports a single condition.
data ObjectFilterNotNot = ObjectFilterNotNot
  { objectFilterNotNotObjectUnderscoreattribute :: Maybe ObjectAttribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectFilterNotNot where
  parseJSON = genericParseJSON optionsObjectFilterNotNot
instance ToJSON ObjectFilterNotNot where
  toJSON = genericToJSON optionsObjectFilterNotNot

optionsObjectFilterNotNot :: Options
optionsObjectFilterNotNot =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectFilterNotNotObjectUnderscoreattribute", "object_attribute")
      ]


-- | 
data ObjectFilterOr = ObjectFilterOr
  { objectFilterOrOr :: Maybe [ObjectFilterAndAndInner] -- ^ Match *any* condition to return results.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectFilterOr where
  parseJSON = genericParseJSON optionsObjectFilterOr
instance ToJSON ObjectFilterOr where
  toJSON = genericToJSON optionsObjectFilterOr

optionsObjectFilterOr :: Options
optionsObjectFilterOr =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectFilterOrOr", "or")
      ]


-- | 
data ObjectID = ObjectID
  { objectIDObjectUnderscoretypeUnderscoreid :: Text -- ^ The object type an object belongs to—like \"Companies\" or \"Accounts\". Object type IDs are string-formatted integers that begin at `1` and increment for each new type.
  , objectIDObjectUnderscoreid :: Text -- ^ The unique identifier for an object. If you use an `object_id` that already exists, we'll update the object accordingly.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectID where
  parseJSON = genericParseJSON optionsObjectID
instance ToJSON ObjectID where
  toJSON = genericToJSON optionsObjectID

optionsObjectID :: Options
optionsObjectID =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectIDObjectUnderscoretypeUnderscoreid", "object_type_id")
      , ("objectIDObjectUnderscoreid", "object_id")
      ]


-- | 
data ObjectIDCreateAndUpdate = ObjectIDCreateAndUpdate
  { objectIDCreateAndUpdateObjectUnderscoretypeUnderscoreid :: Text -- ^ The object type an object belongs to—like \"Companies\" or \"Accounts\". Object type IDs are string-formatted integers that begin at `1` and increment for each new type.
  , objectIDCreateAndUpdateObjectUnderscoreid :: Text -- ^ The unique identifier for an object. If you use an `object_id` that already exists, we'll update the object accordingly.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectIDCreateAndUpdate where
  parseJSON = genericParseJSON optionsObjectIDCreateAndUpdate
instance ToJSON ObjectIDCreateAndUpdate where
  toJSON = genericToJSON optionsObjectIDCreateAndUpdate

optionsObjectIDCreateAndUpdate :: Options
optionsObjectIDCreateAndUpdate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectIDCreateAndUpdateObjectUnderscoretypeUnderscoreid", "object_type_id")
      , ("objectIDCreateAndUpdateObjectUnderscoreid", "object_id")
      ]


-- | 
data ObjectIdentifiers = ObjectIdentifiers
  { objectIdentifiersIdentifiers :: Maybe IdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectIdentifiers where
  parseJSON = genericParseJSON optionsObjectIdentifiers
instance ToJSON ObjectIdentifiers where
  toJSON = genericToJSON optionsObjectIdentifiers

optionsObjectIdentifiers :: Options
optionsObjectIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectIdentifiersIdentifiers", "identifiers")
      ]


-- | The &#x60;action&#x60; determines the type of operation you want to perform with an object. If &#x60;identifiers.object_id&#x60; does not exist, we&#39;ll create a new object; if it exists, we&#39;ll update the object accordingly. 
data ObjectIdentify = ObjectIdentify
  { objectIdentifyIdentifiers :: ObjectIdentifyAllOfIdentifiers -- ^ 
  , objectIdentifyType :: Text -- ^ The operation modifies a single object—non person data.
  , objectIdentifyAction :: Text -- ^ Indicates that the operation will `identify` the the item of the specified `type`.
  , objectIdentifyAttributes :: Maybe Value -- ^ The data that belongs to the object. This is information you might want to associate with people later (through `cio_relationships`). Passing `null` or an empty string removes the attribute from the object. Some attributes have special meaning. Please refer to the list of [reserved attributes](/journeys/objects-create/#reserved-attributes). 
  , objectIdentifyCioUnderscorerelationships :: Maybe [ObjectIdentifyAllOfCioRelationships] -- ^ The people you want to associate with an object. Each object in the array represents a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectIdentify where
  parseJSON = genericParseJSON optionsObjectIdentify
instance ToJSON ObjectIdentify where
  toJSON = genericToJSON optionsObjectIdentify

optionsObjectIdentify :: Options
optionsObjectIdentify =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectIdentifyIdentifiers", "identifiers")
      , ("objectIdentifyType", "type")
      , ("objectIdentifyAction", "action")
      , ("objectIdentifyAttributes", "attributes")
      , ("objectIdentifyCioUnderscorerelationships", "cio_relationships")
      ]


-- | 
data ObjectIdentifyAllOfCioRelationships = ObjectIdentifyAllOfCioRelationships
  { objectIdentifyAllOfCioRelationshipsIdentifiers :: Maybe ObjectIdentifyAllOfIdentifiers1 -- ^ 
  , objectIdentifyAllOfCioRelationshipsRelationshipUnderscoreattributes :: Maybe (Map.Map String Value) -- ^ The attributes associated with a relationship. Passing null or an empty string removes the attribute from the relationship. 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectIdentifyAllOfCioRelationships where
  parseJSON = genericParseJSON optionsObjectIdentifyAllOfCioRelationships
instance ToJSON ObjectIdentifyAllOfCioRelationships where
  toJSON = genericToJSON optionsObjectIdentifyAllOfCioRelationships

optionsObjectIdentifyAllOfCioRelationships :: Options
optionsObjectIdentifyAllOfCioRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectIdentifyAllOfCioRelationshipsIdentifiers", "identifiers")
      , ("objectIdentifyAllOfCioRelationshipsRelationshipUnderscoreattributes", "relationship_attributes")
      ]


-- | The identifiers for a custom object. When identifying a new object, you *must* use both the &#x60;object_type_id&#x60; and &#x60;object_id&#x60; (where &#x60;object_type_id&#x60; is an integer representing the type of object and the &#x60;object_id&#x60; is the individual identifier for the object).  If you&#39;re updating an existing object, you can use either the &#x60;object_type_id&#x60; and &#x60;object_id&#x60; or the &#x60;cio_object_id&#x60; (where &#x60;cio_object_id&#x60; is an immutable unique value that Customer.io sets for an object when you create it). 
data ObjectIdentifyAllOfIdentifiers = ObjectIdentifyAllOfIdentifiers
  { objectIdentifyAllOfIdentifiersObjectUnderscoretypeUnderscoreid :: Text -- ^ The object type an object belongs to—like \"Companies\" or \"Accounts\". Object type IDs are string-formatted integers that begin at `1` and increment for each new type.
  , objectIdentifyAllOfIdentifiersObjectUnderscoreid :: Text -- ^ The unique identifier for an object. If you use an `object_id` that already exists, we'll update the object accordingly.
  , objectIdentifyAllOfIdentifiersCioUnderscoreobjectUnderscoreid :: Text -- ^ A unique value that Customer.io sets for an object when you create it. This ID is immutable.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectIdentifyAllOfIdentifiers where
  parseJSON = genericParseJSON optionsObjectIdentifyAllOfIdentifiers
instance ToJSON ObjectIdentifyAllOfIdentifiers where
  toJSON = genericToJSON optionsObjectIdentifyAllOfIdentifiers

optionsObjectIdentifyAllOfIdentifiers :: Options
optionsObjectIdentifyAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectIdentifyAllOfIdentifiersObjectUnderscoretypeUnderscoreid", "object_type_id")
      , ("objectIdentifyAllOfIdentifiersObjectUnderscoreid", "object_id")
      , ("objectIdentifyAllOfIdentifiersCioUnderscoreobjectUnderscoreid", "cio_object_id")
      ]


-- | 
data ObjectIdentifyAllOfIdentifiers1 = ObjectIdentifyAllOfIdentifiers1
  { objectIdentifyAllOfIdentifiers1Id :: Maybe Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , objectIdentifyAllOfIdentifiers1Email :: Maybe Text -- ^ The email address of the customer.
  , objectIdentifyAllOfIdentifiers1CioUnderscoreid :: Maybe Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectIdentifyAllOfIdentifiers1 where
  parseJSON = genericParseJSON optionsObjectIdentifyAllOfIdentifiers1
instance ToJSON ObjectIdentifyAllOfIdentifiers1 where
  toJSON = genericToJSON optionsObjectIdentifyAllOfIdentifiers1

optionsObjectIdentifyAllOfIdentifiers1 :: Options
optionsObjectIdentifyAllOfIdentifiers1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectIdentifyAllOfIdentifiers1Id", "id")
      , ("objectIdentifyAllOfIdentifiers1Email", "email")
      , ("objectIdentifyAllOfIdentifiers1CioUnderscoreid", "cio_id")
      ]


-- | The &#x60;identify_anonymous&#x60; action lets you relate an object to a person who hasn&#39;t yet identified themselves by anonymous_id. When you identify the person, their anonymous relationship will carry over to the identified profile.
data ObjectIdentifyAnonymous = ObjectIdentifyAnonymous
  { objectIdentifyAnonymousIdentifiers :: ObjectIdentifyAnonymousAllOfIdentifiers -- ^ 
  , objectIdentifyAnonymousType :: Text -- ^ The operation modifies a single object—non person data.
  , objectIdentifyAnonymousAction :: Text -- ^ Indicates that the operation will `identify` the item of the specified `type` and relate it to an `anonymous_id`.
  , objectIdentifyAnonymousAttributes :: Maybe Value -- ^ The data that belongs to the object. This is information you might want to associate with people later (through `cio_relationships`). Passing `null` or an empty string removes the attribute from the object. Some attributes have special meaning. Please refer to the list of [reserved attributes](/journeys/objects-create/#reserved-attributes). 
  , objectIdentifyAnonymousCioUnderscorerelationships :: Maybe [ObjectIdentifyAnonymousAllOfCioRelationships] -- ^ The anonymous people you want to associate with an object. Each object in the array contains an `anonymous_id` representing a person you haven't yet identified by `id` or `email`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectIdentifyAnonymous where
  parseJSON = genericParseJSON optionsObjectIdentifyAnonymous
instance ToJSON ObjectIdentifyAnonymous where
  toJSON = genericToJSON optionsObjectIdentifyAnonymous

optionsObjectIdentifyAnonymous :: Options
optionsObjectIdentifyAnonymous =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectIdentifyAnonymousIdentifiers", "identifiers")
      , ("objectIdentifyAnonymousType", "type")
      , ("objectIdentifyAnonymousAction", "action")
      , ("objectIdentifyAnonymousAttributes", "attributes")
      , ("objectIdentifyAnonymousCioUnderscorerelationships", "cio_relationships")
      ]


-- | 
data ObjectIdentifyAnonymousAllOfCioRelationships = ObjectIdentifyAnonymousAllOfCioRelationships
  { objectIdentifyAnonymousAllOfCioRelationshipsIdentifiers :: Maybe IdentifyAnonymousAllOfCioRelationshipsInnerIdentifiers -- ^ 
  , objectIdentifyAnonymousAllOfCioRelationshipsRelationshipUnderscoreattributes :: Maybe Value -- ^ Coming October 2023 - The attributes associated with a relationship. Passing null or an empty string removes the attribute from the relationship.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectIdentifyAnonymousAllOfCioRelationships where
  parseJSON = genericParseJSON optionsObjectIdentifyAnonymousAllOfCioRelationships
instance ToJSON ObjectIdentifyAnonymousAllOfCioRelationships where
  toJSON = genericToJSON optionsObjectIdentifyAnonymousAllOfCioRelationships

optionsObjectIdentifyAnonymousAllOfCioRelationships :: Options
optionsObjectIdentifyAnonymousAllOfCioRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectIdentifyAnonymousAllOfCioRelationshipsIdentifiers", "identifiers")
      , ("objectIdentifyAnonymousAllOfCioRelationshipsRelationshipUnderscoreattributes", "relationship_attributes")
      ]


-- | The identifiers for a custom object. When identifying a new object, you *must* use both the &#x60;object_type_id&#x60; and &#x60;object_id&#x60; (where &#x60;object_type_id&#x60; is an integer representing the type of object and the &#x60;object_id&#x60; is the individual identifier for the object).  If you&#39;re updating an existing object, you can use either the &#x60;object_type_id&#x60; and &#x60;object_id&#x60; or the &#x60;cio_object_id&#x60; (where &#x60;cio_object_id&#x60; is an immutable unique value that Customer.io sets for an object when you create it). 
data ObjectIdentifyAnonymousAllOfIdentifiers = ObjectIdentifyAnonymousAllOfIdentifiers
  { objectIdentifyAnonymousAllOfIdentifiersObjectUnderscoretypeUnderscoreid :: Text -- ^ The object type an object belongs to—like \"Companies\" or \"Accounts\". Object type IDs are string-formatted integers that begin at `1` and increment for each new type.
  , objectIdentifyAnonymousAllOfIdentifiersObjectUnderscoreid :: Text -- ^ The unique identifier for an object. If you use an `object_id` that already exists, we'll update the object accordingly.
  , objectIdentifyAnonymousAllOfIdentifiersCioUnderscoreobjectUnderscoreid :: Text -- ^ A unique value that Customer.io sets for an object when you create it. This ID is immutable.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectIdentifyAnonymousAllOfIdentifiers where
  parseJSON = genericParseJSON optionsObjectIdentifyAnonymousAllOfIdentifiers
instance ToJSON ObjectIdentifyAnonymousAllOfIdentifiers where
  toJSON = genericToJSON optionsObjectIdentifyAnonymousAllOfIdentifiers

optionsObjectIdentifyAnonymousAllOfIdentifiers :: Options
optionsObjectIdentifyAnonymousAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectIdentifyAnonymousAllOfIdentifiersObjectUnderscoretypeUnderscoreid", "object_type_id")
      , ("objectIdentifyAnonymousAllOfIdentifiersObjectUnderscoreid", "object_id")
      , ("objectIdentifyAnonymousAllOfIdentifiersCioUnderscoreobjectUnderscoreid", "cio_object_id")
      ]


-- | 
data ObjectOperations = ObjectOperations
  { objectOperationsIdentifiers :: IdentifyRequestCioRelationshipsRelationshipsInnerAllOfIdentifiers -- ^ 
  , objectOperationsType :: Text -- ^ The operation modifies a single object—non person data.
  , objectOperationsAction :: Text -- ^ This operation deletes an object relationship from one or more people.
  , objectOperationsAttributes :: Maybe Value -- ^ The data that belongs to the object. This is information you might want to associate with people later (through `cio_relationships`). Passing `null` or an empty string removes the attribute from the object. Some attributes have special meaning. Please refer to the list of [reserved attributes](/journeys/objects-create/#reserved-attributes). 
  , objectOperationsCioUnderscorerelationships :: [Identify1AllOfCioRelationshipsInner] -- ^ The people you want to associate with an object. Each object in the array represents a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectOperations where
  parseJSON = genericParseJSON optionsObjectOperations
instance ToJSON ObjectOperations where
  toJSON = genericToJSON optionsObjectOperations

optionsObjectOperations :: Options
optionsObjectOperations =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectOperationsIdentifiers", "identifiers")
      , ("objectOperationsType", "type")
      , ("objectOperationsAction", "action")
      , ("objectOperationsAttributes", "attributes")
      , ("objectOperationsCioUnderscorerelationships", "cio_relationships")
      ]


-- | Filter your objects by their attributes.
data ObjectattributeFilter = ObjectattributeFilter
  { objectattributeFilterField :: Text -- ^ The name of the attribute you want to filter against.
  , objectattributeFilterOperator :: Text -- ^ Determine how to evaluate criteria against the field—`exists` returns results if an object has the attribute; `eq` returns results an object's attribute exists and the attribute has the `value` you specify.
  , objectattributeFilterValue :: Maybe Text -- ^ The value you want to match for this attribute. You must include a value if you use the `eq` operator.
  , objectattributeFilterTypeUnderscoreid :: Text -- ^ The object type an object belongs to—like \"Companies\" or \"Accounts\". Object type IDs are string-formatted integers that begin at `1` and increment for each new type.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ObjectattributeFilter where
  parseJSON = genericParseJSON optionsObjectattributeFilter
instance ToJSON ObjectattributeFilter where
  toJSON = genericToJSON optionsObjectattributeFilter

optionsObjectattributeFilter :: Options
optionsObjectattributeFilter =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("objectattributeFilterField", "field")
      , ("objectattributeFilterOperator", "operator")
      , ("objectattributeFilterValue", "value")
      , ("objectattributeFilterTypeUnderscoreid", "type_id")
      ]


-- | 
data Or = Or
  { orOr :: Maybe [ComplexAudienceFilterAndInner] -- ^ Match *any* condition to return results.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Or where
  parseJSON = genericParseJSON optionsOr
instance ToJSON Or where
  toJSON = genericToJSON optionsOr

optionsOr :: Options
optionsOr =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("orOr", "or")
      ]


-- | 
data Or1 = Or1
  { or1Or :: Maybe [PeopleFilter] -- ^ Match *any* condition to return results.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Or1 where
  parseJSON = genericParseJSON optionsOr1
instance ToJSON Or1 where
  toJSON = genericToJSON optionsOr1

optionsOr1 :: Options
optionsOr1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("or1Or", "or")
      ]


-- | 
data Or2 = Or2
  { or2Or :: Maybe [OrAudienceFilterOrInner] -- ^ Match *any* condition to return results.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Or2 where
  parseJSON = genericParseJSON optionsOr2
instance ToJSON Or2 where
  toJSON = genericToJSON optionsOr2

optionsOr2 :: Options
optionsOr2 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("or2Or", "or")
      ]


-- | 
data Or3 = Or3
  { or3Or :: Maybe [ObjectFilterAndAndInner] -- ^ Match *any* condition to return results.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Or3 where
  parseJSON = genericParseJSON optionsOr3
instance ToJSON Or3 where
  toJSON = genericToJSON optionsOr3

optionsOr3 :: Options
optionsOr3 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("or3Or", "or")
      ]


-- | 
data OrAudienceFilter = OrAudienceFilter
  { orAudienceFilterOr :: Maybe [OrAudienceFilterOrInner] -- ^ Match *any* condition to return results.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OrAudienceFilter where
  parseJSON = genericParseJSON optionsOrAudienceFilter
instance ToJSON OrAudienceFilter where
  toJSON = genericToJSON optionsOrAudienceFilter

optionsOrAudienceFilter :: Options
optionsOrAudienceFilter =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("orAudienceFilterOr", "or")
      ]


-- | 
data OrAudienceFilterOrInner = OrAudienceFilterOrInner
  { orAudienceFilterOrInnerAnd :: Maybe [ComplexAudienceFilterAndInner] -- ^ Returns results matching *all* conditions.
  , orAudienceFilterOrInnerNot :: Maybe ComplexAudienceFilterNot -- ^ 
  , orAudienceFilterOrInnerSegment :: Maybe Segment -- ^ 
  , orAudienceFilterOrInnerAttribute :: Maybe Attribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OrAudienceFilterOrInner where
  parseJSON = genericParseJSON optionsOrAudienceFilterOrInner
instance ToJSON OrAudienceFilterOrInner where
  toJSON = genericToJSON optionsOrAudienceFilterOrInner

optionsOrAudienceFilterOrInner :: Options
optionsOrAudienceFilterOrInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("orAudienceFilterOrInnerAnd", "and")
      , ("orAudienceFilterOrInnerNot", "not")
      , ("orAudienceFilterOrInnerSegment", "segment")
      , ("orAudienceFilterOrInnerAttribute", "attribute")
      ]


-- | 
data PageView = PageView
  { pageViewName :: Text -- ^ The name of the event. This is how you'll reference the event in campaigns or segments.
  , pageViewId :: Maybe Text -- ^ An identifier used to deduplicate events. This value must be a [ULID](https://github.com/ulid/spec). If an event has the same value as an event we previously received, we won't show or process the duplicate. Note - our Python and Ruby libraries do not pass this id.
  , pageViewType :: Text -- ^ Indicates that the event represents a page view. See [\"page view\" events](/sdk/web/events/#page-view-events), for more information.
  , pageViewTimestamp :: Maybe Int -- ^ The unix timestamp when the event took place. If you don't provide this value, we use the date-time when we receive the event. 
  , pageViewData :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on your customer (referenced by `customer_id`).
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageView where
  parseJSON = genericParseJSON optionsPageView
instance ToJSON PageView where
  toJSON = genericToJSON optionsPageView

optionsPageView :: Options
optionsPageView =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("pageViewName", "name")
      , ("pageViewId", "id")
      , ("pageViewType", "type")
      , ("pageViewTimestamp", "timestamp")
      , ("pageViewData", "data")
      ]


-- | 
data PageView1 = PageView1
  { pageView1AnonymousUnderscoreid :: Maybe Text -- ^ An identifier for an anonymous event, like a cookie. If set as an attribute on a person, any events bearing the same anonymous value are associated with this person. This value must be unique and is not reusable.
  , pageView1Name :: Text -- ^ The name of the event. In general, this should be the URL of the page a person visited, making it easy to segment your audience or trigger campaigns using this event. Make sure you trim leading and trailing spaces from this field.
  , pageView1Id :: Maybe Text -- ^ An identifier used to deduplicate events. This value must be a [ULID](https://github.com/ulid/spec). If an event has the same value as an event we previously received, we won't show or process the duplicate. Note - our Python and Ruby libraries do not pass this id.
  , pageView1Type :: Text -- ^ Indicates that the event represents a page view. See [\"page view\" events](/sdk/web/events/#page-view-events), for more information.
  , pageView1Timestamp :: Maybe Int -- ^ The unix timestamp when the event took place. If you don't provide this value, we use the date-time when we receive the event. 
  , pageView1Data :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on your customer (referenced by `customer_id`).
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PageView1 where
  parseJSON = genericParseJSON optionsPageView1
instance ToJSON PageView1 where
  toJSON = genericToJSON optionsPageView1

optionsPageView1 :: Options
optionsPageView1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("pageView1AnonymousUnderscoreid", "anonymous_id")
      , ("pageView1Name", "name")
      , ("pageView1Id", "id")
      , ("pageView1Type", "type")
      , ("pageView1Timestamp", "timestamp")
      , ("pageView1Data", "data")
      ]


-- | Contains your import parameters.
data People = People
  { peopleName :: Text -- ^ A friendly name for your import. This helps you identify your import.
  , peopleDataUnderscorefileUnderscoreurl :: Text -- ^ The URL or path to the CSV file you want to import.
  , peopleType :: Text -- ^ The type of import.
  , peopleIdentifier :: Text -- ^ The type of identifier you want to use to identify people in your sheet—`id` or `email`. At least one column in the CSV must contain an identifier.
  , peopleDataUnderscoretoUnderscoreprocess :: Maybe Text -- ^ Determines whether your import operation performs `all` add/update operations, only adds items (`only_new`), or only updates existing items (`only_existing`). Defaults to `all`. If `import_type` is `event`, you can only use `all` or `only_existing`.   This field was previously called `people_to_process` - we still support it but will deprecate it soon. 
  , peopleDescription :: Maybe Text -- ^ A helpful description that can help you find and recognize your import operation.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON People where
  parseJSON = genericParseJSON optionsPeople
instance ToJSON People where
  toJSON = genericToJSON optionsPeople

optionsPeople :: Options
optionsPeople =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("peopleName", "name")
      , ("peopleDataUnderscorefileUnderscoreurl", "data_file_url")
      , ("peopleType", "type")
      , ("peopleIdentifier", "identifier")
      , ("peopleDataUnderscoretoUnderscoreprocess", "data_to_process")
      , ("peopleDescription", "description")
      ]


-- | When filtering for people, you can use &#x60;and&#x60; and &#x60;or&#x60; arrays to determine the logic for a group of filter conditions. &#x60;not&#x60; reverses the filter condition and matches when the condition is false. &#x60;segment&#x60; and &#x60;attribute&#x60; represent the individual conditions you can filter a group of people for.
data PeopleFilter = PeopleFilter
  { peopleFilterAnd :: Maybe [ComplexAudienceFilterAndInner] -- ^ Returns results matching *all* conditions.
  , peopleFilterOr :: Maybe [ComplexAudienceFilterAndInner] -- ^ Returns results matching *any* conditions.
  , peopleFilterNot :: Maybe ComplexAudienceFilterNot -- ^ 
  , peopleFilterSegment :: Maybe Segment -- ^ 
  , peopleFilterAttribute :: Maybe Attribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PeopleFilter where
  parseJSON = genericParseJSON optionsPeopleFilter
instance ToJSON PeopleFilter where
  toJSON = genericToJSON optionsPeopleFilter

optionsPeopleFilter :: Options
optionsPeopleFilter =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("peopleFilterAnd", "and")
      , ("peopleFilterOr", "or")
      , ("peopleFilterNot", "not")
      , ("peopleFilterSegment", "segment")
      , ("peopleFilterAttribute", "attribute")
      ]


-- | Returns metrics in accordance with the resolution you requested. Each property in this object is an array and each entry in the array represents a metric period, i.e. if your &#x60;res&#x60; is days &#x60;[0, 1]&#x60; would represent 0 metrics for the first day, 1 for the second, etc.
data PeriodMessageMetrics = PeriodMessageMetrics
  { periodMessageMetricsAttempted :: Maybe [Int] -- ^ The number of `attempted` messages.
  , periodMessageMetricsBounced :: Maybe [Int] -- ^ The number of `bounced` messages.
  , periodMessageMetricsClicked :: Maybe [Int] -- ^ The number of `clicked` messages.
  , periodMessageMetricsHumanUnderscoreclicked :: Maybe [Int] -- ^ The number of `clicked` emails excluding machine clicks. This metric is reliable starting April 20, 2025. [Learn more](/journeys/analytics/#delivery-metrics).
  , periodMessageMetricsPrefetchUnderscoreclicked :: Maybe [Int] -- ^ The number of `clicked` emails attributed to machines. This metric is reliable starting April 20, 2025.
  , periodMessageMetricsConverted :: Maybe [Int] -- ^ The number of `converted` messages.
  , periodMessageMetricsCreated :: Maybe [Int] -- ^ The number of `created` messages.
  , periodMessageMetricsDeferred :: Maybe [Int] -- ^ The number of `deferred` messages.
  , periodMessageMetricsDelivered :: Maybe [Int] -- ^ The number of `delivered` messages.
  , periodMessageMetricsDrafted :: Maybe [Int] -- ^ The number of `drafted` messages.
  , periodMessageMetricsFailed :: Maybe [Int] -- ^ The number of `failed` messages.
  , periodMessageMetricsOpened :: Maybe [Int] -- ^ The number of `opened` messages.
  , periodMessageMetricsHumanUnderscoreopened :: Maybe [Int] -- ^ The number of `opened` emails excluding machine opens. This metric is reliable starting March 20, 2025. [Learn more](/journeys/analytics/#delivery-metrics).
  , periodMessageMetricsPrefetchUnderscoreopened :: Maybe [Int] -- ^ The number of `opened` emails attributed to machines. This metric is reliable starting March 20, 2025.
  , periodMessageMetricsSent :: Maybe [Int] -- ^ The number of sent messages.
  , periodMessageMetricsSpammed :: Maybe [Int] -- ^ The number of spam complaints.
  , periodMessageMetricsSuppressed :: Maybe [Int] -- ^ The number of `suppressed` messages.
  , periodMessageMetricsUndeliverable :: Maybe [Int] -- ^ The number of `undeliverable` messages.
  , periodMessageMetricsTopicUnderscoreunsubscribed :: Maybe [Int] -- ^ The number of topic unsubscribes in a given period.
  , periodMessageMetricsUnsubscribed :: Maybe [Int] -- ^ The number of unsubscribes attributed to the campaign or message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PeriodMessageMetrics where
  parseJSON = genericParseJSON optionsPeriodMessageMetrics
instance ToJSON PeriodMessageMetrics where
  toJSON = genericToJSON optionsPeriodMessageMetrics

optionsPeriodMessageMetrics :: Options
optionsPeriodMessageMetrics =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("periodMessageMetricsAttempted", "attempted")
      , ("periodMessageMetricsBounced", "bounced")
      , ("periodMessageMetricsClicked", "clicked")
      , ("periodMessageMetricsHumanUnderscoreclicked", "human_clicked")
      , ("periodMessageMetricsPrefetchUnderscoreclicked", "prefetch_clicked")
      , ("periodMessageMetricsConverted", "converted")
      , ("periodMessageMetricsCreated", "created")
      , ("periodMessageMetricsDeferred", "deferred")
      , ("periodMessageMetricsDelivered", "delivered")
      , ("periodMessageMetricsDrafted", "drafted")
      , ("periodMessageMetricsFailed", "failed")
      , ("periodMessageMetricsOpened", "opened")
      , ("periodMessageMetricsHumanUnderscoreopened", "human_opened")
      , ("periodMessageMetricsPrefetchUnderscoreopened", "prefetch_opened")
      , ("periodMessageMetricsSent", "sent")
      , ("periodMessageMetricsSpammed", "spammed")
      , ("periodMessageMetricsSuppressed", "suppressed")
      , ("periodMessageMetricsUndeliverable", "undeliverable")
      , ("periodMessageMetricsTopicUnderscoreunsubscribed", "topic_unsubscribed")
      , ("periodMessageMetricsUnsubscribed", "unsubscribed")
      ]


-- | Returns metrics in accordance with the &#x60;period&#x60; you requested. Each property in this object is an array and each entry in the array represents a metric period, i.e. if your &#x60;period&#x60; is days &#x60;[0, 1]&#x60; would represent 0 metrics for the first day, 1 for the second, etc.
data PeriodMessageMetricsDeprecated = PeriodMessageMetricsDeprecated
  { periodMessageMetricsDeprecatedAttempted :: Maybe [Int] -- ^ The number of `attempted` messages.
  , periodMessageMetricsDeprecatedBounced :: Maybe [Int] -- ^ The number of `bounced` messages.
  , periodMessageMetricsDeprecatedClicked :: Maybe [Int] -- ^ The number of `clicked` messages.
  , periodMessageMetricsDeprecatedConverted :: Maybe [Int] -- ^ The number of `converted` messages.
  , periodMessageMetricsDeprecatedCreated :: Maybe [Int] -- ^ The number of `created` messages.
  , periodMessageMetricsDeprecatedDeferred :: Maybe [Int] -- ^ The number of `deferred` messages.
  , periodMessageMetricsDeprecatedDelivered :: Maybe [Int] -- ^ The number of `delivered` messages.
  , periodMessageMetricsDeprecatedDrafted :: Maybe [Int] -- ^ The number of `drafted` messages.
  , periodMessageMetricsDeprecatedFailed :: Maybe [Int] -- ^ The number of `failed` messages.
  , periodMessageMetricsDeprecatedOpened :: Maybe [Int] -- ^ The number of `opened` messages.
  , periodMessageMetricsDeprecatedSent :: Maybe [Int] -- ^ The number of sent messages.
  , periodMessageMetricsDeprecatedSpammed :: Maybe [Int] -- ^ The number of spam complaints.
  , periodMessageMetricsDeprecatedSuppressed :: Maybe [Int] -- ^ The number of `suppressed` messages.
  , periodMessageMetricsDeprecatedUndeliverable :: Maybe [Int] -- ^ The number of `undeliverable` messages.
  , periodMessageMetricsDeprecatedTopicUnderscoreunsubscribed :: Maybe [Int] -- ^ The number of topic unsubscribes in a given period.
  , periodMessageMetricsDeprecatedUnsubscribed :: Maybe [Int] -- ^ The number of unsubscribes attributed to the campaign or message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PeriodMessageMetricsDeprecated where
  parseJSON = genericParseJSON optionsPeriodMessageMetricsDeprecated
instance ToJSON PeriodMessageMetricsDeprecated where
  toJSON = genericToJSON optionsPeriodMessageMetricsDeprecated

optionsPeriodMessageMetricsDeprecated :: Options
optionsPeriodMessageMetricsDeprecated =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("periodMessageMetricsDeprecatedAttempted", "attempted")
      , ("periodMessageMetricsDeprecatedBounced", "bounced")
      , ("periodMessageMetricsDeprecatedClicked", "clicked")
      , ("periodMessageMetricsDeprecatedConverted", "converted")
      , ("periodMessageMetricsDeprecatedCreated", "created")
      , ("periodMessageMetricsDeprecatedDeferred", "deferred")
      , ("periodMessageMetricsDeprecatedDelivered", "delivered")
      , ("periodMessageMetricsDeprecatedDrafted", "drafted")
      , ("periodMessageMetricsDeprecatedFailed", "failed")
      , ("periodMessageMetricsDeprecatedOpened", "opened")
      , ("periodMessageMetricsDeprecatedSent", "sent")
      , ("periodMessageMetricsDeprecatedSpammed", "spammed")
      , ("periodMessageMetricsDeprecatedSuppressed", "suppressed")
      , ("periodMessageMetricsDeprecatedUndeliverable", "undeliverable")
      , ("periodMessageMetricsDeprecatedTopicUnderscoreunsubscribed", "topic_unsubscribed")
      , ("periodMessageMetricsDeprecatedUnsubscribed", "unsubscribed")
      ]


-- | Returns metrics in accordance with the resolution you requested. Each property in this object is an array and each entry in the array represents a metric period, i.e. if your &#x60;res&#x60; is days &#x60;[0, 1]&#x60; would represent 0 metrics for the first day, 1 for the second, etc.
data PeriodWebhookMetrics = PeriodWebhookMetrics
  { periodWebhookMetrics2xx :: Maybe [Int] -- ^ 2xx responses by period, representative of webhook performance.
  , periodWebhookMetrics3xx :: Maybe [Int] -- ^ 3xx responses by period, representative of webhook performance.
  , periodWebhookMetrics4xx :: Maybe [Int] -- ^ 4xx responses by period, representative of webhook performance.
  , periodWebhookMetrics5xx :: Maybe [Int] -- ^ 5xx responses by period, representative of webhook performance.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PeriodWebhookMetrics where
  parseJSON = genericParseJSON optionsPeriodWebhookMetrics
instance ToJSON PeriodWebhookMetrics where
  toJSON = genericToJSON optionsPeriodWebhookMetrics

optionsPeriodWebhookMetrics :: Options
optionsPeriodWebhookMetrics =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("periodWebhookMetrics2xx", "2xx")
      , ("periodWebhookMetrics3xx", "3xx")
      , ("periodWebhookMetrics4xx", "4xx")
      , ("periodWebhookMetrics5xx", "5xx")
      ]


-- | Returns metrics in accordance with the &#x60;period&#x60; you requested. Each property in this object is an array and each entry in the array represents a metric period, i.e. if your &#x60;period&#x60; is days &#x60;[0, 1]&#x60; would represent 0 metrics for the first day, 1 for the second, etc.
data PeriodWebhookMetricsDeprecated = PeriodWebhookMetricsDeprecated
  { periodWebhookMetricsDeprecated2xx :: Maybe [Int] -- ^ 2xx responses by period, representative of webhook performance.
  , periodWebhookMetricsDeprecated3xx :: Maybe [Int] -- ^ 3xx responses by period, representative of webhook performance.
  , periodWebhookMetricsDeprecated4xx :: Maybe [Int] -- ^ 4xx responses by period, representative of webhook performance.
  , periodWebhookMetricsDeprecated5xx :: Maybe [Int] -- ^ 5xx responses by period, representative of webhook performance.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PeriodWebhookMetricsDeprecated where
  parseJSON = genericParseJSON optionsPeriodWebhookMetricsDeprecated
instance ToJSON PeriodWebhookMetricsDeprecated where
  toJSON = genericToJSON optionsPeriodWebhookMetricsDeprecated

optionsPeriodWebhookMetricsDeprecated :: Options
optionsPeriodWebhookMetricsDeprecated =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("periodWebhookMetricsDeprecated2xx", "2xx")
      , ("periodWebhookMetricsDeprecated3xx", "3xx")
      , ("periodWebhookMetricsDeprecated4xx", "4xx")
      , ("periodWebhookMetricsDeprecated5xx", "5xx")
      ]


-- | 
data Person = Person
  { personType :: Text -- ^ The operation modifies a person in Customer.io
  , personIdentifiers :: IdentifyAllOfIdentifiers -- ^ 
  , personAction :: Text -- ^ Unsuppress a person's identifier(s) in Customer.io, so that you can message a person or add their identifiers back to your workspace. This does not unsuppress addresses that were previously suppressed by your email provider.
  , personAttributes :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on the identified person.
  , personCioUnderscorerelationships :: [IdentifyRequestCioRelationshipsRelationshipsInner] -- ^ Each object in the array represents a relationship you want to add to, or remove from, a person.
  , personId :: Maybe Text -- ^ A valid ULID used to deduplicate events. Note - our Python and Ruby libraries do not pass this id.
  , personName :: Text -- ^ The name of the page or page path that a person visited. This is how you'll find and select page view events in Customer.io.
  , personTimestamp :: Maybe Int -- ^ The Unix timestamp when the event happened.
  , personDevice :: DeleteDeviceAllOfDevice -- ^ 
  , personPrimary :: PersonOneOf3Primary -- ^ 
  , personSecondary :: PersonOneOf3Secondary -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Person where
  parseJSON = genericParseJSON optionsPerson
instance ToJSON Person where
  toJSON = genericToJSON optionsPerson

optionsPerson :: Options
optionsPerson =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personType", "type")
      , ("personIdentifiers", "identifiers")
      , ("personAction", "action")
      , ("personAttributes", "attributes")
      , ("personCioUnderscorerelationships", "cio_relationships")
      , ("personId", "id")
      , ("personName", "name")
      , ("personTimestamp", "timestamp")
      , ("personDevice", "device")
      , ("personPrimary", "primary")
      , ("personSecondary", "secondary")
      ]


-- | 
data Person1 = Person1
  { person1Type :: Text -- ^ The operation modifies a person in Customer.io
  , person1Identifiers :: IdentifyAllOfIdentifiers -- ^ 
  , person1Action :: Text -- ^ Unsuppress a person's identifier(s) in Customer.io, so that you can message a person or add their identifiers back to your workspace. This does not unsuppress addresses that were previously suppressed by your email provider.
  , person1Attributes :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on the identified person.
  , person1CioUnderscorerelationships :: [IdentifyRequestCioRelationshipsRelationshipsInner] -- ^ Each object in the array represents a relationship you want to add to, or remove from, a person.
  , person1Id :: Maybe Text -- ^ A valid ULID used to deduplicate events. Note - our Python and Ruby libraries do not pass this id.
  , person1Name :: Text -- ^ The name of the page or page path that a person visited. This is how you'll find and select page view events in Customer.io.
  , person1Timestamp :: Maybe Int -- ^ The Unix timestamp when the event happened.
  , person1Device :: DeleteDeviceAllOfDevice -- ^ 
  , person1Primary :: PersonOneOf3Primary -- ^ 
  , person1Secondary :: PersonOneOf3Secondary -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Person1 where
  parseJSON = genericParseJSON optionsPerson1
instance ToJSON Person1 where
  toJSON = genericToJSON optionsPerson1

optionsPerson1 :: Options
optionsPerson1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("person1Type", "type")
      , ("person1Identifiers", "identifiers")
      , ("person1Action", "action")
      , ("person1Attributes", "attributes")
      , ("person1CioUnderscorerelationships", "cio_relationships")
      , ("person1Id", "id")
      , ("person1Name", "name")
      , ("person1Timestamp", "timestamp")
      , ("person1Device", "device")
      , ("person1Primary", "primary")
      , ("person1Secondary", "secondary")
      ]


-- | Assign devices to a person.
data PersonAddDevice = PersonAddDevice
  { personAddDeviceType :: Text -- ^ The operation modifies a person in Customer.io
  , personAddDeviceIdentifiers :: PersonAddDeviceAllOfIdentifiers -- ^ 
  , personAddDeviceAction :: Text -- ^ Add a mobile device to a person's profile.
  , personAddDeviceDevice :: PersonAddDeviceAllOfDevice -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonAddDevice where
  parseJSON = genericParseJSON optionsPersonAddDevice
instance ToJSON PersonAddDevice where
  toJSON = genericToJSON optionsPersonAddDevice

optionsPersonAddDevice :: Options
optionsPersonAddDevice =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personAddDeviceType", "type")
      , ("personAddDeviceIdentifiers", "identifiers")
      , ("personAddDeviceAction", "action")
      , ("personAddDeviceDevice", "device")
      ]


-- | The properties representing an individual device. [Our SDK&#39;s](/sdk/) gather all the properties defined below automatically, unless you disable the &#x60;autoTrackDeviceAttributes&#x60; setting. You can reference the properties outside the &#x60;attributes&#x60; object in segments.
data PersonAddDeviceAllOfDevice = PersonAddDeviceAllOfDevice
  { personAddDeviceAllOfDeviceToken :: Text -- ^ The device token.
  , personAddDeviceAllOfDeviceLastUnderscoreused :: Maybe Int -- ^ The `timestamp` when you last identified this device. If you don't pass a timestamp when you add or update a device, we use the time of the request itself. Our SDKs identify a device when a person launches their app.
  , personAddDeviceAllOfDevicePlatform :: Text -- ^ The device/messaging platform.
  , personAddDeviceAllOfDeviceAttributes :: Maybe AddDeviceRequestDeviceAllOfAttributes -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonAddDeviceAllOfDevice where
  parseJSON = genericParseJSON optionsPersonAddDeviceAllOfDevice
instance ToJSON PersonAddDeviceAllOfDevice where
  toJSON = genericToJSON optionsPersonAddDeviceAllOfDevice

optionsPersonAddDeviceAllOfDevice :: Options
optionsPersonAddDeviceAllOfDevice =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personAddDeviceAllOfDeviceToken", "token")
      , ("personAddDeviceAllOfDeviceLastUnderscoreused", "last_used")
      , ("personAddDeviceAllOfDevicePlatform", "platform")
      , ("personAddDeviceAllOfDeviceAttributes", "attributes")
      ]


-- | The person you want to perform an action for—one of either &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. You cannot pass multiple identifiers.
data PersonAddDeviceAllOfIdentifiers = PersonAddDeviceAllOfIdentifiers
  { personAddDeviceAllOfIdentifiersId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , personAddDeviceAllOfIdentifiersEmail :: Text -- ^ The email address of the customer.
  , personAddDeviceAllOfIdentifiersCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonAddDeviceAllOfIdentifiers where
  parseJSON = genericParseJSON optionsPersonAddDeviceAllOfIdentifiers
instance ToJSON PersonAddDeviceAllOfIdentifiers where
  toJSON = genericToJSON optionsPersonAddDeviceAllOfIdentifiers

optionsPersonAddDeviceAllOfIdentifiers :: Options
optionsPersonAddDeviceAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personAddDeviceAllOfIdentifiersId", "id")
      , ("personAddDeviceAllOfIdentifiersEmail", "email")
      , ("personAddDeviceAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | Associate multiple objects with a person.
data PersonAddRelationships = PersonAddRelationships
  { personAddRelationshipsType :: Text -- ^ The operation modifies a person in Customer.io
  , personAddRelationshipsIdentifiers :: PersonAddRelationshipsAllOfIdentifiers -- ^ 
  , personAddRelationshipsAction :: Text -- ^ This operation associates a person with one or more objects.
  , personAddRelationshipsCioUnderscorerelationships :: [PersonAddRelationshipsAllOfCioRelationships] -- ^ Each object in the array represents a relationship you want to add to, or remove from, a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonAddRelationships where
  parseJSON = genericParseJSON optionsPersonAddRelationships
instance ToJSON PersonAddRelationships where
  toJSON = genericToJSON optionsPersonAddRelationships

optionsPersonAddRelationships :: Options
optionsPersonAddRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personAddRelationshipsType", "type")
      , ("personAddRelationshipsIdentifiers", "identifiers")
      , ("personAddRelationshipsAction", "action")
      , ("personAddRelationshipsCioUnderscorerelationships", "cio_relationships")
      ]


-- | 
data PersonAddRelationshipsAllOfCioRelationships = PersonAddRelationshipsAllOfCioRelationships
  { personAddRelationshipsAllOfCioRelationshipsIdentifiers :: Maybe ObjectCommonAllOfIdentifiers -- ^ 
  , personAddRelationshipsAllOfCioRelationshipsRelationshipUnderscoreattributes :: Maybe (Map.Map String Value) -- ^ The attributes associated with a relationship. Passing null or an empty string removes the attribute from the relationship. 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonAddRelationshipsAllOfCioRelationships where
  parseJSON = genericParseJSON optionsPersonAddRelationshipsAllOfCioRelationships
instance ToJSON PersonAddRelationshipsAllOfCioRelationships where
  toJSON = genericToJSON optionsPersonAddRelationshipsAllOfCioRelationships

optionsPersonAddRelationshipsAllOfCioRelationships :: Options
optionsPersonAddRelationshipsAllOfCioRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personAddRelationshipsAllOfCioRelationshipsIdentifiers", "identifiers")
      , ("personAddRelationshipsAllOfCioRelationshipsRelationshipUnderscoreattributes", "relationship_attributes")
      ]


-- | The person you want to perform an action for—one of either &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. You cannot pass multiple identifiers.
data PersonAddRelationshipsAllOfIdentifiers = PersonAddRelationshipsAllOfIdentifiers
  { personAddRelationshipsAllOfIdentifiersId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , personAddRelationshipsAllOfIdentifiersEmail :: Text -- ^ The email address of the customer.
  , personAddRelationshipsAllOfIdentifiersCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonAddRelationshipsAllOfIdentifiers where
  parseJSON = genericParseJSON optionsPersonAddRelationshipsAllOfIdentifiers
instance ToJSON PersonAddRelationshipsAllOfIdentifiers where
  toJSON = genericToJSON optionsPersonAddRelationshipsAllOfIdentifiers

optionsPersonAddRelationshipsAllOfIdentifiers :: Options
optionsPersonAddRelationshipsAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personAddRelationshipsAllOfIdentifiersId", "id")
      , ("personAddRelationshipsAllOfIdentifiersEmail", "email")
      , ("personAddRelationshipsAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | Attributes that you want to add or update for this person.
data PersonAttributes = PersonAttributes
  { personAttributesCioUnderscoresubscriptionUnderscorepreferences :: Maybe IdentifyAllOfAttributesCioSubscriptionPreferences -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonAttributes where
  parseJSON = genericParseJSON optionsPersonAttributes
instance ToJSON PersonAttributes where
  toJSON = genericToJSON optionsPersonAttributes

optionsPersonAttributes :: Options
optionsPersonAttributes =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personAttributesCioUnderscoresubscriptionUnderscorepreferences", "cio_subscription_preferences")
      ]


-- | 
data PersonCommon = PersonCommon
  { personCommonType :: Text -- ^ The operation modifies a person in Customer.io
  , personCommonIdentifiers :: IdentifyAllOfIdentifiers -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonCommon where
  parseJSON = genericParseJSON optionsPersonCommon
instance ToJSON PersonCommon where
  toJSON = genericToJSON optionsPersonCommon

optionsPersonCommon :: Options
optionsPersonCommon =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personCommonType", "type")
      , ("personCommonIdentifiers", "identifiers")
      ]


-- | Delete a person from your workspace.
data PersonDelete = PersonDelete
  { personDeleteType :: Text -- ^ The operation modifies a person in Customer.io
  , personDeleteIdentifiers :: PersonDeleteAllOfIdentifiers -- ^ 
  , personDeleteAction :: Text -- ^ Indicates that the operation will `delete` the the item of the specified `type`.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonDelete where
  parseJSON = genericParseJSON optionsPersonDelete
instance ToJSON PersonDelete where
  toJSON = genericToJSON optionsPersonDelete

optionsPersonDelete :: Options
optionsPersonDelete =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personDeleteType", "type")
      , ("personDeleteIdentifiers", "identifiers")
      , ("personDeleteAction", "action")
      ]


-- | The person you want to perform an action for—one of either &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. You cannot pass multiple identifiers.
data PersonDeleteAllOfIdentifiers = PersonDeleteAllOfIdentifiers
  { personDeleteAllOfIdentifiersId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , personDeleteAllOfIdentifiersEmail :: Text -- ^ The email address of the customer.
  , personDeleteAllOfIdentifiersCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonDeleteAllOfIdentifiers where
  parseJSON = genericParseJSON optionsPersonDeleteAllOfIdentifiers
instance ToJSON PersonDeleteAllOfIdentifiers where
  toJSON = genericToJSON optionsPersonDeleteAllOfIdentifiers

optionsPersonDeleteAllOfIdentifiers :: Options
optionsPersonDeleteAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personDeleteAllOfIdentifiersId", "id")
      , ("personDeleteAllOfIdentifiersEmail", "email")
      , ("personDeleteAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | Delete devices that belong to a person.
data PersonDeleteDevice = PersonDeleteDevice
  { personDeleteDeviceType :: Text -- ^ The operation modifies a person in Customer.io
  , personDeleteDeviceIdentifiers :: PersonDeleteDeviceAllOfIdentifiers -- ^ 
  , personDeleteDeviceAction :: Text -- ^ Delete a device from a person's profile.
  , personDeleteDeviceDevice :: DeleteDeviceAllOfDevice -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonDeleteDevice where
  parseJSON = genericParseJSON optionsPersonDeleteDevice
instance ToJSON PersonDeleteDevice where
  toJSON = genericToJSON optionsPersonDeleteDevice

optionsPersonDeleteDevice :: Options
optionsPersonDeleteDevice =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personDeleteDeviceType", "type")
      , ("personDeleteDeviceIdentifiers", "identifiers")
      , ("personDeleteDeviceAction", "action")
      , ("personDeleteDeviceDevice", "device")
      ]


-- | The person you want to perform an action for—one of either &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. You cannot pass multiple identifiers.
data PersonDeleteDeviceAllOfIdentifiers = PersonDeleteDeviceAllOfIdentifiers
  { personDeleteDeviceAllOfIdentifiersId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , personDeleteDeviceAllOfIdentifiersEmail :: Text -- ^ The email address of the customer.
  , personDeleteDeviceAllOfIdentifiersCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonDeleteDeviceAllOfIdentifiers where
  parseJSON = genericParseJSON optionsPersonDeleteDeviceAllOfIdentifiers
instance ToJSON PersonDeleteDeviceAllOfIdentifiers where
  toJSON = genericToJSON optionsPersonDeleteDeviceAllOfIdentifiers

optionsPersonDeleteDeviceAllOfIdentifiers :: Options
optionsPersonDeleteDeviceAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personDeleteDeviceAllOfIdentifiersId", "id")
      , ("personDeleteDeviceAllOfIdentifiersEmail", "email")
      , ("personDeleteDeviceAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | Remove multiple object relationships from a person.
data PersonDeleteRelationships = PersonDeleteRelationships
  { personDeleteRelationshipsType :: Text -- ^ The operation modifies a person in Customer.io
  , personDeleteRelationshipsIdentifiers :: PersonDeleteRelationshipsAllOfIdentifiers -- ^ 
  , personDeleteRelationshipsAction :: Text -- ^ This operation deletes an object relationship from one or more people.
  , personDeleteRelationshipsCioUnderscorerelationships :: [PersonDeleteRelationshipsAllOfCioRelationships] -- ^ Each object in the array represents a relationship you want to add to, or remove from, a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonDeleteRelationships where
  parseJSON = genericParseJSON optionsPersonDeleteRelationships
instance ToJSON PersonDeleteRelationships where
  toJSON = genericToJSON optionsPersonDeleteRelationships

optionsPersonDeleteRelationships :: Options
optionsPersonDeleteRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personDeleteRelationshipsType", "type")
      , ("personDeleteRelationshipsIdentifiers", "identifiers")
      , ("personDeleteRelationshipsAction", "action")
      , ("personDeleteRelationshipsCioUnderscorerelationships", "cio_relationships")
      ]


-- | 
data PersonDeleteRelationshipsAllOfCioRelationships = PersonDeleteRelationshipsAllOfCioRelationships
  { personDeleteRelationshipsAllOfCioRelationshipsIdentifiers :: Maybe ObjectCommonAllOfIdentifiers -- ^ 
  , personDeleteRelationshipsAllOfCioRelationshipsRelationshipUnderscoreattributes :: Maybe (Map.Map String Value) -- ^ The attributes associated with a relationship. Passing null or an empty string removes the attribute from the relationship. 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonDeleteRelationshipsAllOfCioRelationships where
  parseJSON = genericParseJSON optionsPersonDeleteRelationshipsAllOfCioRelationships
instance ToJSON PersonDeleteRelationshipsAllOfCioRelationships where
  toJSON = genericToJSON optionsPersonDeleteRelationshipsAllOfCioRelationships

optionsPersonDeleteRelationshipsAllOfCioRelationships :: Options
optionsPersonDeleteRelationshipsAllOfCioRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personDeleteRelationshipsAllOfCioRelationshipsIdentifiers", "identifiers")
      , ("personDeleteRelationshipsAllOfCioRelationshipsRelationshipUnderscoreattributes", "relationship_attributes")
      ]


-- | The person you want to perform an action for—one of either &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. You cannot pass multiple identifiers.
data PersonDeleteRelationshipsAllOfIdentifiers = PersonDeleteRelationshipsAllOfIdentifiers
  { personDeleteRelationshipsAllOfIdentifiersId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , personDeleteRelationshipsAllOfIdentifiersEmail :: Text -- ^ The email address of the customer.
  , personDeleteRelationshipsAllOfIdentifiersCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonDeleteRelationshipsAllOfIdentifiers where
  parseJSON = genericParseJSON optionsPersonDeleteRelationshipsAllOfIdentifiers
instance ToJSON PersonDeleteRelationshipsAllOfIdentifiers where
  toJSON = genericToJSON optionsPersonDeleteRelationshipsAllOfIdentifiers

optionsPersonDeleteRelationshipsAllOfIdentifiers :: Options
optionsPersonDeleteRelationshipsAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personDeleteRelationshipsAllOfIdentifiersId", "id")
      , ("personDeleteRelationshipsAllOfIdentifiersEmail", "email")
      , ("personDeleteRelationshipsAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | A custom event attributed to a person. You can use events to trigger campaigns, or reference event information using liquid in your messages.
data PersonEvent = PersonEvent
  { personEventType :: Text -- ^ The operation modifies a person in Customer.io
  , personEventIdentifiers :: PersonEventAllOfIdentifiers -- ^ 
  , personEventAction :: Text -- ^ A custom event attributed to the specified person.
  , personEventId :: Maybe Text -- ^ A valid ULID used to deduplicate events. Note - our Python and Ruby libraries do not pass this id.
  , personEventName :: Text -- ^ The name of the event. This is how you'll find your event in Customer.io or select it when using events as campaign triggers.
  , personEventTimestamp :: Maybe Int -- ^ The Unix timestamp when the event happened.
  , personEventAttributes :: Maybe PersonOneOfAllOfAttributes -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonEvent where
  parseJSON = genericParseJSON optionsPersonEvent
instance ToJSON PersonEvent where
  toJSON = genericToJSON optionsPersonEvent

optionsPersonEvent :: Options
optionsPersonEvent =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personEventType", "type")
      , ("personEventIdentifiers", "identifiers")
      , ("personEventAction", "action")
      , ("personEventId", "id")
      , ("personEventName", "name")
      , ("personEventTimestamp", "timestamp")
      , ("personEventAttributes", "attributes")
      ]


-- | The person you want to perform an action for—one of either &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. You cannot pass multiple identifiers.
data PersonEventAllOfIdentifiers = PersonEventAllOfIdentifiers
  { personEventAllOfIdentifiersId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , personEventAllOfIdentifiersEmail :: Text -- ^ The email address of the customer.
  , personEventAllOfIdentifiersCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonEventAllOfIdentifiers where
  parseJSON = genericParseJSON optionsPersonEventAllOfIdentifiers
instance ToJSON PersonEventAllOfIdentifiers where
  toJSON = genericToJSON optionsPersonEventAllOfIdentifiers

optionsPersonEventAllOfIdentifiers :: Options
optionsPersonEventAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personEventAllOfIdentifiersId", "id")
      , ("personEventAllOfIdentifiersEmail", "email")
      , ("personEventAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | Merge two people. You&#39;ll merge the &#x60;secondary&#x60; person into the &#x60;primary&#x60;. The primary profile remains after the merge and the secondary is deleted. This operation is _not_ reversible. See our page on [merging duplicate people](/merge-people/) for more information. 
data PersonMerge = PersonMerge
  { personMergeType :: Text -- ^ The operation modifies a person in Customer.io
  , personMergeAction :: Text -- ^ Merge two people. You'll merge the `secondary` person into the `primary`. The primary profile remains after the merge and the secondary is deleted. This operation is _not_ reversible. See our page on [merging duplicate people](/merge-people/) for more information. 
  , personMergePrimary :: PersonOneOf3Primary -- ^ 
  , personMergeSecondary :: PersonOneOf3Secondary -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonMerge where
  parseJSON = genericParseJSON optionsPersonMerge
instance ToJSON PersonMerge where
  toJSON = genericToJSON optionsPersonMerge

optionsPersonMerge :: Options
optionsPersonMerge =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personMergeType", "type")
      , ("personMergeAction", "action")
      , ("personMergePrimary", "primary")
      , ("personMergeSecondary", "secondary")
      ]


-- | A custom event attributed to a person. You can use events to trigger campaigns, or reference event information using liquid in your messages.
data PersonOneOf = PersonOneOf
  { personOneOfType :: Text -- ^ The operation modifies a person in Customer.io
  , personOneOfIdentifiers :: IdentifyAllOfIdentifiers -- ^ 
  , personOneOfAction :: Text -- ^ A custom event attributed to the specified person.
  , personOneOfId :: Maybe Text -- ^ A valid ULID used to deduplicate events. Note - our Python and Ruby libraries do not pass this id.
  , personOneOfName :: Text -- ^ The name of the event. This is how you'll find your event in Customer.io or select it when using events as campaign triggers.
  , personOneOfTimestamp :: Maybe Int -- ^ The Unix timestamp when the event happened.
  , personOneOfAttributes :: Maybe PersonOneOfAllOfAttributes -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonOneOf where
  parseJSON = genericParseJSON optionsPersonOneOf
instance ToJSON PersonOneOf where
  toJSON = genericToJSON optionsPersonOneOf

optionsPersonOneOf :: Options
optionsPersonOneOf =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personOneOfType", "type")
      , ("personOneOfIdentifiers", "identifiers")
      , ("personOneOfAction", "action")
      , ("personOneOfId", "id")
      , ("personOneOfName", "name")
      , ("personOneOfTimestamp", "timestamp")
      , ("personOneOfAttributes", "attributes")
      ]


-- | A mobile \&quot;screenview\&quot; event attributed to a person. Our &#x60;screen&#x60; and &#x60;page&#x60; event types are more specific than our standard &#x60;event&#x60;, and help you track and target people based on the pages people visit in your mobile app or website.
data PersonOneOf1 = PersonOneOf1
  { personOneOf1Type :: Text -- ^ The operation modifies a person in Customer.io
  , personOneOf1Identifiers :: IdentifyAllOfIdentifiers -- ^ 
  , personOneOf1Action :: Text -- ^ A mobile \"screenview\" event attributed to a person. Our `screen` and `page` event types are more specific than our standard `event`, and help you track and target people based on the pages people visit in your mobile app or website.
  , personOneOf1Id :: Maybe Text -- ^ A valid ULID used to deduplicate events. Note - our Python and Ruby libraries do not pass this id.
  , personOneOf1Name :: Text -- ^ The name of the screen a person visited. This is how you'll find and select screen view events in Customer.io.
  , personOneOf1Timestamp :: Maybe Int -- ^ The Unix timestamp when the event happened.
  , personOneOf1Attributes :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on the identified person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonOneOf1 where
  parseJSON = genericParseJSON optionsPersonOneOf1
instance ToJSON PersonOneOf1 where
  toJSON = genericToJSON optionsPersonOneOf1

optionsPersonOneOf1 :: Options
optionsPersonOneOf1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personOneOf1Type", "type")
      , ("personOneOf1Identifiers", "identifiers")
      , ("personOneOf1Action", "action")
      , ("personOneOf1Id", "id")
      , ("personOneOf1Name", "name")
      , ("personOneOf1Timestamp", "timestamp")
      , ("personOneOf1Attributes", "attributes")
      ]


-- | A web \&quot;pageview\&quot; event attributed to a person. Our &#x60;screen&#x60; and &#x60;page&#x60; event types are more specific than our standard &#x60;event&#x60;, and help you track and target people based on the pages people visit in your mobile app or website.
data PersonOneOf2 = PersonOneOf2
  { personOneOf2Type :: Text -- ^ The operation modifies a person in Customer.io
  , personOneOf2Identifiers :: IdentifyAllOfIdentifiers -- ^ 
  , personOneOf2Action :: Text -- ^ A web \"pageview\" event attributed to a person. Our `screen` and `page` event types are more specific than our standard `event`, and help you track and target people based on the pages people visit in your mobile app or website.
  , personOneOf2Id :: Maybe Text -- ^ A valid ULID used to deduplicate events. Note - our Python and Ruby libraries do not pass this id.
  , personOneOf2Name :: Text -- ^ The name of the page or page path that a person visited. This is how you'll find and select page view events in Customer.io.
  , personOneOf2Timestamp :: Maybe Int -- ^ The Unix timestamp when the event happened.
  , personOneOf2Attributes :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on the identified person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonOneOf2 where
  parseJSON = genericParseJSON optionsPersonOneOf2
instance ToJSON PersonOneOf2 where
  toJSON = genericToJSON optionsPersonOneOf2

optionsPersonOneOf2 :: Options
optionsPersonOneOf2 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personOneOf2Type", "type")
      , ("personOneOf2Identifiers", "identifiers")
      , ("personOneOf2Action", "action")
      , ("personOneOf2Id", "id")
      , ("personOneOf2Name", "name")
      , ("personOneOf2Timestamp", "timestamp")
      , ("personOneOf2Attributes", "attributes")
      ]


-- | Merge two people. You&#39;ll merge the &#x60;secondary&#x60; person into the &#x60;primary&#x60;. The primary profile remains after the merge and the secondary is deleted. This operation is _not_ reversible. See our page on [merging duplicate people](/merge-people/) for more information. 
data PersonOneOf3 = PersonOneOf3
  { personOneOf3Type :: Text -- ^ The operation modifies a person in Customer.io
  , personOneOf3Action :: Text -- ^ Merge two people. You'll merge the `secondary` person into the `primary`. The primary profile remains after the merge and the secondary is deleted. This operation is _not_ reversible. See our page on [merging duplicate people](/merge-people/) for more information. 
  , personOneOf3Primary :: PersonOneOf3Primary -- ^ 
  , personOneOf3Secondary :: PersonOneOf3Secondary -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonOneOf3 where
  parseJSON = genericParseJSON optionsPersonOneOf3
instance ToJSON PersonOneOf3 where
  toJSON = genericToJSON optionsPersonOneOf3

optionsPersonOneOf3 :: Options
optionsPersonOneOf3 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personOneOf3Type", "type")
      , ("personOneOf3Action", "action")
      , ("personOneOf3Primary", "primary")
      , ("personOneOf3Secondary", "secondary")
      ]


-- | The person that you want to remain after the merge, identified by one of &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. This person receives information from the secondary person in the merge.           If email is disabled as an identifier in your [workspace settings](https://fly.customer.io/workspaces/last/settings/edit), then you must reference people by &#x60;id&#x60; or &#x60;cio_id&#x60;. Under How to Modify, &#x60;id&#x60; must be set to \&quot;Reference people by cio_id\&quot; for a successful merge.  
data PersonOneOf3Primary = PersonOneOf3Primary
  { personOneOf3PrimaryId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , personOneOf3PrimaryEmail :: Text -- ^ The email address of the customer.
  , personOneOf3PrimaryCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonOneOf3Primary where
  parseJSON = genericParseJSON optionsPersonOneOf3Primary
instance ToJSON PersonOneOf3Primary where
  toJSON = genericToJSON optionsPersonOneOf3Primary

optionsPersonOneOf3Primary :: Options
optionsPersonOneOf3Primary =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personOneOf3PrimaryId", "id")
      , ("personOneOf3PrimaryEmail", "email")
      , ("personOneOf3PrimaryCioUnderscoreid", "cio_id")
      ]


-- | The person that you want to delete after the merge, identified by one of &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. This person&#39;s information is merged into the primary person&#39;s profile and then it is deleted.        If email is disabled as an identifier in your [workspace settings](https://fly.customer.io/workspaces/last/settings/edit), then you must reference people by &#x60;id&#x60; or &#x60;cio_id&#x60;. Under How to Modify, &#x60;id&#x60; must be set to \&quot;Reference people by cio_id\&quot; for a successful merge. 
data PersonOneOf3Secondary = PersonOneOf3Secondary
  { personOneOf3SecondaryId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , personOneOf3SecondaryEmail :: Text -- ^ The email address of the customer.
  , personOneOf3SecondaryCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonOneOf3Secondary where
  parseJSON = genericParseJSON optionsPersonOneOf3Secondary
instance ToJSON PersonOneOf3Secondary where
  toJSON = genericToJSON optionsPersonOneOf3Secondary

optionsPersonOneOf3Secondary :: Options
optionsPersonOneOf3Secondary =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personOneOf3SecondaryId", "id")
      , ("personOneOf3SecondaryEmail", "email")
      , ("personOneOf3SecondaryCioUnderscoreid", "cio_id")
      ]


-- | Additional information that you might want to reference in a message using liquid or use to set attributes on the identified person. 
newtype PersonOneOfAllOfAttributes = PersonOneOfAllOfAttributes { unPersonOneOfAllOfAttributes :: (Map.Map Text Value) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data PersonOperations = PersonOperations
  { personOperationsType :: Text -- ^ The operation modifies a person in Customer.io
  , personOperationsIdentifiers :: IdentifyAllOfIdentifiers -- ^ 
  , personOperationsAction :: Text -- ^ Unsuppress a person's identifier(s) in Customer.io, so that you can message a person or add their identifiers back to your workspace. This does not unsuppress addresses that were previously suppressed by your email provider.
  , personOperationsAttributes :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on the identified person.
  , personOperationsCioUnderscorerelationships :: [IdentifyRequestCioRelationshipsRelationshipsInner] -- ^ Each object in the array represents a relationship you want to add to, or remove from, a person.
  , personOperationsId :: Maybe Text -- ^ A valid ULID used to deduplicate events. Note - our Python and Ruby libraries do not pass this id.
  , personOperationsName :: Text -- ^ The name of the page or page path that a person visited. This is how you'll find and select page view events in Customer.io.
  , personOperationsTimestamp :: Maybe Int -- ^ The Unix timestamp when the event happened.
  , personOperationsDevice :: DeleteDeviceAllOfDevice -- ^ 
  , personOperationsPrimary :: PersonOneOf3Primary -- ^ 
  , personOperationsSecondary :: PersonOneOf3Secondary -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonOperations where
  parseJSON = genericParseJSON optionsPersonOperations
instance ToJSON PersonOperations where
  toJSON = genericToJSON optionsPersonOperations

optionsPersonOperations :: Options
optionsPersonOperations =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personOperationsType", "type")
      , ("personOperationsIdentifiers", "identifiers")
      , ("personOperationsAction", "action")
      , ("personOperationsAttributes", "attributes")
      , ("personOperationsCioUnderscorerelationships", "cio_relationships")
      , ("personOperationsId", "id")
      , ("personOperationsName", "name")
      , ("personOperationsTimestamp", "timestamp")
      , ("personOperationsDevice", "device")
      , ("personOperationsPrimary", "primary")
      , ("personOperationsSecondary", "secondary")
      ]


-- | A web \&quot;pageview\&quot; event attributed to a person. Our &#x60;screen&#x60; and &#x60;page&#x60; event types are more specific than our standard &#x60;event&#x60;, and help you track and target people based on the pages people visit in your mobile app or website.
data PersonPage = PersonPage
  { personPageType :: Text -- ^ The operation modifies a person in Customer.io
  , personPageIdentifiers :: PersonPageAllOfIdentifiers -- ^ 
  , personPageAction :: Text -- ^ A web \"pageview\" event attributed to a person. Our `screen` and `page` event types are more specific than our standard `event`, and help you track and target people based on the pages people visit in your mobile app or website.
  , personPageId :: Maybe Text -- ^ A valid ULID used to deduplicate events. Note - our Python and Ruby libraries do not pass this id.
  , personPageName :: Text -- ^ The name of the page or page path that a person visited. This is how you'll find and select page view events in Customer.io.
  , personPageTimestamp :: Maybe Int -- ^ The Unix timestamp when the event happened.
  , personPageAttributes :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on the identified person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonPage where
  parseJSON = genericParseJSON optionsPersonPage
instance ToJSON PersonPage where
  toJSON = genericToJSON optionsPersonPage

optionsPersonPage :: Options
optionsPersonPage =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personPageType", "type")
      , ("personPageIdentifiers", "identifiers")
      , ("personPageAction", "action")
      , ("personPageId", "id")
      , ("personPageName", "name")
      , ("personPageTimestamp", "timestamp")
      , ("personPageAttributes", "attributes")
      ]


-- | The person you want to perform an action for—one of either &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. You cannot pass multiple identifiers.
data PersonPageAllOfIdentifiers = PersonPageAllOfIdentifiers
  { personPageAllOfIdentifiersId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , personPageAllOfIdentifiersEmail :: Text -- ^ The email address of the customer.
  , personPageAllOfIdentifiersCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonPageAllOfIdentifiers where
  parseJSON = genericParseJSON optionsPersonPageAllOfIdentifiers
instance ToJSON PersonPageAllOfIdentifiers where
  toJSON = genericToJSON optionsPersonPageAllOfIdentifiers

optionsPersonPageAllOfIdentifiers :: Options
optionsPersonPageAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personPageAllOfIdentifiersId", "id")
      , ("personPageAllOfIdentifiersEmail", "email")
      , ("personPageAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | A mobile \&quot;screenview\&quot; event attributed to a person. Our &#x60;screen&#x60; and &#x60;page&#x60; event types are more specific than our standard &#x60;event&#x60;, and help you track and target people based on the pages people visit in your mobile app or website.
data PersonScreen = PersonScreen
  { personScreenType :: Text -- ^ The operation modifies a person in Customer.io
  , personScreenIdentifiers :: PersonScreenAllOfIdentifiers -- ^ 
  , personScreenAction :: Text -- ^ A mobile \"screenview\" event attributed to a person. Our `screen` and `page` event types are more specific than our standard `event`, and help you track and target people based on the pages people visit in your mobile app or website.
  , personScreenId :: Maybe Text -- ^ A valid ULID used to deduplicate events. Note - our Python and Ruby libraries do not pass this id.
  , personScreenName :: Text -- ^ The name of the screen a person visited. This is how you'll find and select screen view events in Customer.io.
  , personScreenTimestamp :: Maybe Int -- ^ The Unix timestamp when the event happened.
  , personScreenAttributes :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on the identified person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonScreen where
  parseJSON = genericParseJSON optionsPersonScreen
instance ToJSON PersonScreen where
  toJSON = genericToJSON optionsPersonScreen

optionsPersonScreen :: Options
optionsPersonScreen =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personScreenType", "type")
      , ("personScreenIdentifiers", "identifiers")
      , ("personScreenAction", "action")
      , ("personScreenId", "id")
      , ("personScreenName", "name")
      , ("personScreenTimestamp", "timestamp")
      , ("personScreenAttributes", "attributes")
      ]


-- | The person you want to perform an action for—one of either &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. You cannot pass multiple identifiers.
data PersonScreenAllOfIdentifiers = PersonScreenAllOfIdentifiers
  { personScreenAllOfIdentifiersId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , personScreenAllOfIdentifiersEmail :: Text -- ^ The email address of the customer.
  , personScreenAllOfIdentifiersCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonScreenAllOfIdentifiers where
  parseJSON = genericParseJSON optionsPersonScreenAllOfIdentifiers
instance ToJSON PersonScreenAllOfIdentifiers where
  toJSON = genericToJSON optionsPersonScreenAllOfIdentifiers

optionsPersonScreenAllOfIdentifiers :: Options
optionsPersonScreenAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personScreenAllOfIdentifiersId", "id")
      , ("personScreenAllOfIdentifiersEmail", "email")
      , ("personScreenAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | Suppress a person&#39;s identifier(s) in Customer.io, so that you can&#39;t message a person or add their identifiers back to your workspace. This is separate from suppressions performed by your email provider.
data PersonSuppress = PersonSuppress
  { personSuppressType :: Text -- ^ The operation modifies a person in Customer.io
  , personSuppressIdentifiers :: PersonSuppressAllOfIdentifiers -- ^ 
  , personSuppressAction :: Text -- ^ Suppress a person's identifier(s) in Customer.io, so that you can't message a person or add their identifiers back to your workspace. This is separate from suppressions performed by your email provider.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonSuppress where
  parseJSON = genericParseJSON optionsPersonSuppress
instance ToJSON PersonSuppress where
  toJSON = genericToJSON optionsPersonSuppress

optionsPersonSuppress :: Options
optionsPersonSuppress =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personSuppressType", "type")
      , ("personSuppressIdentifiers", "identifiers")
      , ("personSuppressAction", "action")
      ]


-- | The person you want to perform an action for—one of either &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. You cannot pass multiple identifiers.
data PersonSuppressAllOfIdentifiers = PersonSuppressAllOfIdentifiers
  { personSuppressAllOfIdentifiersId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , personSuppressAllOfIdentifiersEmail :: Text -- ^ The email address of the customer.
  , personSuppressAllOfIdentifiersCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonSuppressAllOfIdentifiers where
  parseJSON = genericParseJSON optionsPersonSuppressAllOfIdentifiers
instance ToJSON PersonSuppressAllOfIdentifiers where
  toJSON = genericToJSON optionsPersonSuppressAllOfIdentifiers

optionsPersonSuppressAllOfIdentifiers :: Options
optionsPersonSuppressAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personSuppressAllOfIdentifiersId", "id")
      , ("personSuppressAllOfIdentifiersEmail", "email")
      , ("personSuppressAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | Unsuppress a person&#39;s identifier(s) in Customer.io, so that you can message a person or add their identifiers back to your workspace. This does not unsuppress addresses that were previously suppressed by your email provider.
data PersonUnsuppress = PersonUnsuppress
  { personUnsuppressType :: Text -- ^ The operation modifies a person in Customer.io
  , personUnsuppressIdentifiers :: PersonUnsuppressAllOfIdentifiers -- ^ 
  , personUnsuppressAction :: Text -- ^ Unsuppress a person's identifier(s) in Customer.io, so that you can message a person or add their identifiers back to your workspace. This does not unsuppress addresses that were previously suppressed by your email provider.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonUnsuppress where
  parseJSON = genericParseJSON optionsPersonUnsuppress
instance ToJSON PersonUnsuppress where
  toJSON = genericToJSON optionsPersonUnsuppress

optionsPersonUnsuppress :: Options
optionsPersonUnsuppress =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personUnsuppressType", "type")
      , ("personUnsuppressIdentifiers", "identifiers")
      , ("personUnsuppressAction", "action")
      ]


-- | The person you want to perform an action for—one of either &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;. You cannot pass multiple identifiers.
data PersonUnsuppressAllOfIdentifiers = PersonUnsuppressAllOfIdentifiers
  { personUnsuppressAllOfIdentifiersId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI.
  , personUnsuppressAllOfIdentifiersEmail :: Text -- ^ The email address of the customer.
  , personUnsuppressAllOfIdentifiersCioUnderscoreid :: Text -- ^ A unique identifier set by Customer.io, used to reference a person if you want to update their identifiers.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PersonUnsuppressAllOfIdentifiers where
  parseJSON = genericParseJSON optionsPersonUnsuppressAllOfIdentifiers
instance ToJSON PersonUnsuppressAllOfIdentifiers where
  toJSON = genericToJSON optionsPersonUnsuppressAllOfIdentifiers

optionsPersonUnsuppressAllOfIdentifiers :: Options
optionsPersonUnsuppressAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("personUnsuppressAllOfIdentifiersId", "id")
      , ("personUnsuppressAllOfIdentifiersEmail", "email")
      , ("personUnsuppressAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | By default, we process CSS before emails leave Customer.io using Premailer. If your message included CSS and pre-processing is not disabled, this key indicates the pre-processor.
data Preprocessor = Preprocessor
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Preprocessor where
  parseJSON = genericParseJSON optionsPreprocessor
instance ToJSON Preprocessor where
  toJSON = genericToJSON optionsPreprocessor

optionsPreprocessor :: Options
optionsPreprocessor =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data Push = Push
  { pushDeliveryUnderscoreid :: Text -- ^ The CIO-Delivery-ID from the notification that you want to associate the `event` with.
  , pushTimestamp :: Maybe Int -- ^ The unix timestamp when the event occurred.
  , pushMetric :: Text -- ^ The type of device-side event you want to report back to Customer.io.
  , pushRecipient :: Maybe Text -- ^ The device ID that the message was sent to.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Push where
  parseJSON = genericParseJSON optionsPush
instance ToJSON Push where
  toJSON = genericToJSON optionsPush

optionsPush :: Options
optionsPush =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("pushDeliveryUnderscoreid", "delivery_id")
      , ("pushTimestamp", "timestamp")
      , ("pushMetric", "metric")
      , ("pushRecipient", "recipient")
      ]


-- | Describes the push events reported from Customer.io to a webhook.
data PushEvents = PushEvents
  { pushEventsPushUnderscoreattempted :: Maybe Bool -- ^ Reports when a push notification could not be sent to the delivery provider will retry. Set to true to report this event type.
  , pushEventsPushUnderscorebounced :: Maybe Bool -- ^ Reports when the delivery provider is unable to deliver a message. Set to true to report this event type.
  , pushEventsPushUnderscoreclicked :: Maybe Bool -- ^ Reports when a person clicks a tracked link in a message. Set to true to report this event type.
  , pushEventsPushUnderscoreconverted :: Maybe Bool -- ^ Reports a conversion. Set to true to report this event type.
  , pushEventsPushUnderscoredelivered :: Maybe Bool -- ^ An app reports that the recipient's device received a message. Set to true to report this event type.
  , pushEventsPushUnderscoredrafted :: Maybe Bool -- ^ Reports when a message draft is created. Set to true to report this event type.
  , pushEventsPushUnderscoredropped :: Maybe Bool -- ^ Reports when a message isn't sent because the recipient is suppressed. Set to true to report this event type.
  , pushEventsPushUnderscorefailed :: Maybe Bool -- ^ Reports when a message couldn't be sent to the delivery provider. Set to true to report this event type.
  , pushEventsPushUnderscoreopened :: Maybe Bool -- ^ The app on a recipient's device reports that the recipient opened the message. Set to true to report this event type.
  , pushEventsPushUnderscoresent :: Maybe Bool -- ^ Reports when a message is sent from Customer.io to the delivery provider. Set to true to report this event type.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PushEvents where
  parseJSON = genericParseJSON optionsPushEvents
instance ToJSON PushEvents where
  toJSON = genericToJSON optionsPushEvents

optionsPushEvents :: Options
optionsPushEvents =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("pushEventsPushUnderscoreattempted", "push_attempted")
      , ("pushEventsPushUnderscorebounced", "push_bounced")
      , ("pushEventsPushUnderscoreclicked", "push_clicked")
      , ("pushEventsPushUnderscoreconverted", "push_converted")
      , ("pushEventsPushUnderscoredelivered", "push_delivered")
      , ("pushEventsPushUnderscoredrafted", "push_drafted")
      , ("pushEventsPushUnderscoredropped", "push_dropped")
      , ("pushEventsPushUnderscorefailed", "push_failed")
      , ("pushEventsPushUnderscoreopened", "push_opened")
      , ("pushEventsPushUnderscoresent", "push_sent")
      ]


-- | 
data PushMetricsRequest = PushMetricsRequest
  { pushMetricsRequestDeliveryUnderscoreid :: Maybe Text -- ^ The CIO-Delivery-ID from the notification that you want to associate the `event` with.
  , pushMetricsRequestEvent :: Maybe Text -- ^ The type of device-side event you want to report back to Customer.io.
  , pushMetricsRequestDeviceUnderscoreid :: Maybe Text -- ^ The CIO-Delivery-Token representing the device that received the original notification.
  , pushMetricsRequestTimestamp :: Maybe Int -- ^ The unix timestamp when the event occurred.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PushMetricsRequest where
  parseJSON = genericParseJSON optionsPushMetricsRequest
instance ToJSON PushMetricsRequest where
  toJSON = genericToJSON optionsPushMetricsRequest

optionsPushMetricsRequest :: Options
optionsPushMetricsRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("pushMetricsRequestDeliveryUnderscoreid", "delivery_id")
      , ("pushMetricsRequestEvent", "event")
      , ("pushMetricsRequestDeviceUnderscoreid", "device_id")
      , ("pushMetricsRequestTimestamp", "timestamp")
      ]


-- | 
data Relationship = Relationship
  { relationshipId :: Maybe Int -- ^ The identifier for a campaign.
  , relationshipDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , relationshipName :: Maybe Text -- ^ The name of the campaign.
  , relationshipType :: Maybe Text -- ^ The type of campaign trigger. **Sunsetting on March 30, 2025**
  , relationshipCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , relationshipUpdated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , relationshipActive :: Maybe Bool -- ^ If true, the campaign is active and can still send messages.
  , relationshipState :: Maybe Text -- ^ The status of the campaign.
  , relationshipActions :: Maybe [SegmentActionsInner] -- ^ An array of actions contained within the campaign.
  , relationshipFirstUnderscorestarted :: Maybe Int -- ^ The date and time when you first started the campaign and it first became eligible to be triggered.
  , relationshipTags :: Maybe [Text] -- ^ An array of tags you set on this campaign.
  , relationshipFilterUnderscoresegmentUnderscoreids :: Maybe [Int] -- ^ A list of segments used in the campaign filter, returned if the campaign audience was filtered on one or more segments.
  , relationshipObjectUnderscoretypeUnderscoreid :: Maybe Int -- ^ The the object type ID of the trigger.
  , relationshipFilterUnderscoreobjectUnderscoreattributes :: Maybe Text -- ^ A list of object attributes used in the campaign filter, returned if the campaign audience was filtered on one or more object attributes.
  , relationshipFilterUnderscorerelationshipUnderscoreattributes :: Maybe Text -- ^ A list of relationship attributes used in the campaign filter, returned if the campaign audience was filtered on one or more relationship attributes.
  , relationshipAudience :: Maybe RelationshipAudience -- ^ 
  , relationshipRelationshipUnderscoreattributeUnderscoretriggers :: Maybe Value -- ^ A list of relationship attributes used to trigger the campaign.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Relationship where
  parseJSON = genericParseJSON optionsRelationship
instance ToJSON Relationship where
  toJSON = genericToJSON optionsRelationship

optionsRelationship :: Options
optionsRelationship =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("relationshipId", "id")
      , ("relationshipDeduplicateUnderscoreid", "deduplicate_id")
      , ("relationshipName", "name")
      , ("relationshipType", "type")
      , ("relationshipCreated", "created")
      , ("relationshipUpdated", "updated")
      , ("relationshipActive", "active")
      , ("relationshipState", "state")
      , ("relationshipActions", "actions")
      , ("relationshipFirstUnderscorestarted", "first_started")
      , ("relationshipTags", "tags")
      , ("relationshipFilterUnderscoresegmentUnderscoreids", "filter_segment_ids")
      , ("relationshipObjectUnderscoretypeUnderscoreid", "object_type_id")
      , ("relationshipFilterUnderscoreobjectUnderscoreattributes", "filter_object_attributes")
      , ("relationshipFilterUnderscorerelationshipUnderscoreattributes", "filter_relationship_attributes")
      , ("relationshipAudience", "audience")
      , ("relationshipRelationshipUnderscoreattributeUnderscoretriggers", "relationship_attribute_triggers")
      ]


-- | Defines the people who will start a journey in your campaign.
data RelationshipAudience = RelationshipAudience
  { relationshipAudienceType :: Maybe Int -- ^ The type of audience selected. \"Person added to object\" is `0`. \"Every person in the object\" is `1`. \"Certain people in the object\" is also `1`. \"Certain people\" will always have one or more audience filters (see below). \"Every person\" will never have an audience filter.
  , relationshipAudiencePersonUnderscorefilters :: Maybe Value -- ^ Returns the profile attributes you filtered the audience by, if any. Only applies to `type 1`, \"Certain people in the object\".
  , relationshipAudienceRelationshipUnderscorefilters :: Maybe Value -- ^ Returns the relationship attributes you filtered the audience by, if any. Only applies to `type 1`, \"Certain people in the object\".
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RelationshipAudience where
  parseJSON = genericParseJSON optionsRelationshipAudience
instance ToJSON RelationshipAudience where
  toJSON = genericToJSON optionsRelationshipAudience

optionsRelationshipAudience :: Options
optionsRelationshipAudience =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("relationshipAudienceType", "type")
      , ("relationshipAudiencePersonUnderscorefilters", "person_filters")
      , ("relationshipAudienceRelationshipUnderscorefilters", "relationship_filters")
      ]


-- | The IDs of people you want to remove from the segment.
data RemoveFromSegmentRequest = RemoveFromSegmentRequest
  { removeFromSegmentRequestIds :: [Text] -- ^ The customer IDs you want to remove from the segment.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RemoveFromSegmentRequest where
  parseJSON = genericParseJSON optionsRemoveFromSegmentRequest
instance ToJSON RemoveFromSegmentRequest where
  toJSON = genericToJSON optionsRemoveFromSegmentRequest

optionsRemoveFromSegmentRequest :: Options
optionsRemoveFromSegmentRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("removeFromSegmentRequestIds", "ids")
      ]


-- | 
data ReportingWebhook = ReportingWebhook
  { reportingWebhookName :: Text -- ^ The name of your webhook.
  , reportingWebhookId :: Maybe Int -- ^ The identifier for the webhook.
  , reportingWebhookEndpoint :: Text -- ^ The webhook URL.
  , reportingWebhookDisabled :: Maybe Bool -- ^ Set to `true` to quit sending events to the webhook URL. Set to `false` to enable the webhook.
  , reportingWebhookFullUnderscoreresolution :: Maybe Bool -- ^ Set to `false` to send unique open and click events to the webhook. Set to `true` to send all events.
  , reportingWebhookWithUnderscorecontent :: Maybe Bool -- ^ Set to `true` to include the message `body` in `_sent` events.
  , reportingWebhookEvents :: [Text] -- ^ Specifies the types of events you want to report to your webhook. See our [reporting webhooks reference](/api/webhooks/#operation/reportingWebhook) for more information about event types and the information they return.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReportingWebhook where
  parseJSON = genericParseJSON optionsReportingWebhook
instance ToJSON ReportingWebhook where
  toJSON = genericToJSON optionsReportingWebhook

optionsReportingWebhook :: Options
optionsReportingWebhook =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("reportingWebhookName", "name")
      , ("reportingWebhookId", "id")
      , ("reportingWebhookEndpoint", "endpoint")
      , ("reportingWebhookDisabled", "disabled")
      , ("reportingWebhookFullUnderscoreresolution", "full_resolution")
      , ("reportingWebhookWithUnderscorecontent", "with_content")
      , ("reportingWebhookEvents", "events")
      ]


-- | The method used in conjunction with a webhook &#x60;url&#x60;.
data RequestMethod = RequestMethod
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RequestMethod where
  parseJSON = genericParseJSON optionsRequestMethod
instance ToJSON RequestMethod where
  toJSON = genericToJSON optionsRequestMethod

optionsRequestMethod :: Options
optionsRequestMethod =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data SDKIntegration = SDKIntegration
  { sDKIntegrationMessage :: SDKIntegrationMessage -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SDKIntegration where
  parseJSON = genericParseJSON optionsSDKIntegration
instance ToJSON SDKIntegration where
  toJSON = genericToJSON optionsSDKIntegration

optionsSDKIntegration :: Options
optionsSDKIntegration =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("sDKIntegrationMessage", "message")
      ]


-- | 
data SDKIntegrationMessage = SDKIntegrationMessage
  { sDKIntegrationMessageMessage :: FcmAndroidWithSdkMessage -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SDKIntegrationMessage where
  parseJSON = genericParseJSON optionsSDKIntegrationMessage
instance ToJSON SDKIntegrationMessage where
  toJSON = genericToJSON optionsSDKIntegrationMessage

optionsSDKIntegrationMessage :: Options
optionsSDKIntegrationMessage =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("sDKIntegrationMessageMessage", "message")
      ]


-- | 
data SMS = SMS
  { sMSDeliveryUnderscoreid :: Text -- ^ The CIO-Delivery-ID from the notification that you want to associate the `event` with.
  , sMSTimestamp :: Maybe Int -- ^ The unix timestamp when the event occurred.
  , sMSMetric :: Text -- ^ The SMS metric you want to report back to Customer.io.
  , sMSRecipient :: Maybe Int -- ^ The phone number of the person who received the message.
  , sMSReason :: Maybe Text -- ^ For metrics indicating a failure (like `bounced`), this field provides the reason for the failure.
  , sMSHref :: Maybe Text -- ^ For `clicked` metrics, this is the link the recipient clicked.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SMS where
  parseJSON = genericParseJSON optionsSMS
instance ToJSON SMS where
  toJSON = genericToJSON optionsSMS

optionsSMS :: Options
optionsSMS =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("sMSDeliveryUnderscoreid", "delivery_id")
      , ("sMSTimestamp", "timestamp")
      , ("sMSMetric", "metric")
      , ("sMSRecipient", "recipient")
      , ("sMSReason", "reason")
      , ("sMSHref", "href")
      ]


-- | 
data SMSMMS = SMSMMS
  { sMSMMSId :: Maybe Int -- ^ The identifier for an action.
  , sMSMMSCampaignUnderscoreid :: Maybe Int -- ^ The identifier for a campaign.
  , sMSMMSParentUnderscoreactionUnderscoreid :: Maybe Int -- ^ The ID of the parent action, if the action occurred within a campaign and has a parent (like a randomized split, etc).
  , sMSMMSDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , sMSMMSName :: Maybe Text -- ^ The name of the action.
  , sMSMMSLayout :: Maybe Text -- ^ The layout used for the action, if it exists.
  , sMSMMSCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , sMSMMSUpdated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , sMSMMSBody :: Maybe Text -- ^ The body of your SMS message.
  , sMSMMSLanguage :: Maybe Text -- ^ The language variant for your message. If you don't use our [localization feature](/localization), or this is the default message, this value is an empty string.
  , sMSMMSType :: Maybe Text -- ^ For SMS/MMS messages, the `type` is always `twilio`.
  , sMSMMSImageUnderscoreurl :: Maybe Text -- ^ The URL of the image in your SMS (MMS) message.
  , sMSMMSSendingUnderscorestate :: Maybe Text -- ^ Determines the sending behavior for the action. `automatic` sends the action automatically when triggered; `draft` queues drafts when the action is triggered; or `off` to disable the action.
  , sMSMMSRecipient :: Maybe Text -- ^ The recipient value. In general, your recipient is an attribute that you reference using liquid, like `{{customer.phone}}`, instead of a hard-coded value. If you set this field to a liquid statement like `{{customer.phone}}`, the field returns blank in `GET` requests because we populate the recipient from your liquid statement at send time.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SMSMMS where
  parseJSON = genericParseJSON optionsSMSMMS
instance ToJSON SMSMMS where
  toJSON = genericToJSON optionsSMSMMS

optionsSMSMMS :: Options
optionsSMSMMS =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("sMSMMSId", "id")
      , ("sMSMMSCampaignUnderscoreid", "campaign_id")
      , ("sMSMMSParentUnderscoreactionUnderscoreid", "parent_action_id")
      , ("sMSMMSDeduplicateUnderscoreid", "deduplicate_id")
      , ("sMSMMSName", "name")
      , ("sMSMMSLayout", "layout")
      , ("sMSMMSCreated", "created")
      , ("sMSMMSUpdated", "updated")
      , ("sMSMMSBody", "body")
      , ("sMSMMSLanguage", "language")
      , ("sMSMMSType", "type")
      , ("sMSMMSImageUnderscoreurl", "image_url")
      , ("sMSMMSSendingUnderscorestate", "sending_state")
      , ("sMSMMSRecipient", "recipient")
      ]


-- | 
data Segment = Segment
  { segmentId :: Maybe Int -- ^ The identifier for a campaign.
  , segmentDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , segmentName :: Maybe Text -- ^ The name of the campaign.
  , segmentType :: Maybe Text -- ^ The type of campaign trigger. **Sunsetting on March 30, 2025**
  , segmentCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , segmentUpdated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , segmentActive :: Maybe Bool -- ^ If true, the campaign is active and can still send messages.
  , segmentState :: Maybe Text -- ^ The status of the campaign.
  , segmentActions :: Maybe [SegmentActionsInner] -- ^ An array of actions contained within the campaign.
  , segmentFirstUnderscorestarted :: Maybe Int -- ^ The date and time when you first started the campaign and it first became eligible to be triggered.
  , segmentTags :: Maybe [Text] -- ^ An array of tags you set on this campaign.
  , segmentTriggerUnderscoresegmentUnderscoreids :: Maybe [Int] -- ^ A list of segments used in the campaign trigger, returned if the campaign trigger included one or more segment conditions.
  , segmentFilterUnderscoresegmentUnderscoreids :: Maybe [Int] -- ^ A list of segments used in the campaign filter, returned if the campaign audience was filtered on one or more segments.
  , segmentMsgUnderscoretemplates :: Maybe [SegmentMsgTemplatesInner] -- ^ Indicates the message templates used in this campaign.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Segment where
  parseJSON = genericParseJSON optionsSegment
instance ToJSON Segment where
  toJSON = genericToJSON optionsSegment

optionsSegment :: Options
optionsSegment =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("segmentId", "id")
      , ("segmentDeduplicateUnderscoreid", "deduplicate_id")
      , ("segmentName", "name")
      , ("segmentType", "type")
      , ("segmentCreated", "created")
      , ("segmentUpdated", "updated")
      , ("segmentActive", "active")
      , ("segmentState", "state")
      , ("segmentActions", "actions")
      , ("segmentFirstUnderscorestarted", "first_started")
      , ("segmentTags", "tags")
      , ("segmentTriggerUnderscoresegmentUnderscoreids", "trigger_segment_ids")
      , ("segmentFilterUnderscoresegmentUnderscoreids", "filter_segment_ids")
      , ("segmentMsgUnderscoretemplates", "msg_templates")
      ]


-- | 
data Segment1 = Segment1
  { segment1Segment :: Maybe Segment -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Segment1 where
  parseJSON = genericParseJSON optionsSegment1
instance ToJSON Segment1 where
  toJSON = genericToJSON optionsSegment1

optionsSegment1 :: Options
optionsSegment1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("segment1Segment", "segment")
      ]


-- | 
data SegmentActionsInner = SegmentActionsInner
  { segmentActionsInnerType :: Maybe Text -- ^ The action type.
  , segmentActionsInnerId :: Maybe Int -- ^ The identifier for the action.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SegmentActionsInner where
  parseJSON = genericParseJSON optionsSegmentActionsInner
instance ToJSON SegmentActionsInner where
  toJSON = genericToJSON optionsSegmentActionsInner

optionsSegmentActionsInner :: Options
optionsSegmentActionsInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("segmentActionsInnerType", "type")
      , ("segmentActionsInnerId", "id")
      ]


-- | Provide the &#x60;id&#x60; of a segment containing people you want to search for.
data SegmentAudienceFilter = SegmentAudienceFilter
  { segmentAudienceFilterId :: Maybe Int -- ^ The ID of the segment you want to return people from.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SegmentAudienceFilter where
  parseJSON = genericParseJSON optionsSegmentAudienceFilter
instance ToJSON SegmentAudienceFilter where
  toJSON = genericToJSON optionsSegmentAudienceFilter

optionsSegmentAudienceFilter :: Options
optionsSegmentAudienceFilter =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("segmentAudienceFilterId", "id")
      ]


-- | 
data SegmentMsgTemplatesInner = SegmentMsgTemplatesInner
  { segmentMsgTemplatesInnerType :: Maybe Text -- ^ The message type the template represents.
  , segmentMsgTemplatesInnerId :: Maybe Int -- ^ The identifier for the template.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SegmentMsgTemplatesInner where
  parseJSON = genericParseJSON optionsSegmentMsgTemplatesInner
instance ToJSON SegmentMsgTemplatesInner where
  toJSON = genericToJSON optionsSegmentMsgTemplatesInner

optionsSegmentMsgTemplatesInner :: Options
optionsSegmentMsgTemplatesInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("segmentMsgTemplatesInnerType", "type")
      , ("segmentMsgTemplatesInnerId", "id")
      ]


-- | 
data SegmentResponseObject = SegmentResponseObject
  { segmentResponseObjectId :: Maybe Int -- ^ The identifier for a segment.
  , segmentResponseObjectDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , segmentResponseObjectName :: Maybe Text -- ^ The name of the segment.
  , segmentResponseObjectDescription :: Maybe Text -- ^ A description for the segment. This can help you understand the purpose of the segment when you encounter it in other requests or in the UI.
  , segmentResponseObjectState :: Maybe Text -- ^ The state of the segment.  `events` - currently handling event conditions for this segment  `build` - currently handling profile attribute conditions for this segment  `events_queued` - waiting for a process to start handling event conditions for this segment  `build_queued` - waiting for a process to start handling profile attribute conditions for this segment  `finished` - the segment is finished building 
  , segmentResponseObjectProgress :: Maybe Int -- ^ If Customer.io has not finished processing the segment, this indicates the percentage complete. Otherwise, this key is null.
  , segmentResponseObjectType :: Maybe Text -- ^ The type of segment.
  , segmentResponseObjectTags :: Maybe [Text] -- ^ The tags assigned to the segment, if any. Tags may help you sort through your segments.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SegmentResponseObject where
  parseJSON = genericParseJSON optionsSegmentResponseObject
instance ToJSON SegmentResponseObject where
  toJSON = genericToJSON optionsSegmentResponseObject

optionsSegmentResponseObject :: Options
optionsSegmentResponseObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("segmentResponseObjectId", "id")
      , ("segmentResponseObjectDeduplicateUnderscoreid", "deduplicate_id")
      , ("segmentResponseObjectName", "name")
      , ("segmentResponseObjectDescription", "description")
      , ("segmentResponseObjectState", "state")
      , ("segmentResponseObjectProgress", "progress")
      , ("segmentResponseObjectType", "type")
      , ("segmentResponseObjectTags", "tags")
      ]


-- | The payload of a transactional message.
data Sendemail = Sendemail
  { sendemailTransactionalUnderscoremessageUnderscoreid :: SendpushAllOfTransactionalMessageId -- ^ 
  , sendemailBody :: Text -- ^ The body of your message.
  , sendemailBodyUnderscoreamp :: Maybe Text -- ^ If your message is an email, this is the AMP-enabled body of your message. If your recipient's email client doesn't support AMP, the `body` represents your fallback message.
  , sendemailBodyUnderscoreplain :: Maybe Text -- ^ By default, we generate a plaintext version of your message body for each delivery. Use this key to override the default plain text body.
  , sendemailSubject :: Text -- ^ The subject line for your message.
  , sendemailFrom :: Text -- ^ The address that your email is from. This address must be verified by Customer.io. You can include a display/friendly name in your from address in the format `Person <person@example.com>`.
  , sendemailLanguage :: Maybe Text -- ^ Overrides language preferences for the person you want to send your transactional message to. Use one of our [supported two- or four-letter language codes](/localization-getting-started/#supported-languages).
  , sendemailIdentifiers :: SendpushAllOfIdentifiers -- ^ 
  , sendemailMessageUnderscoredata :: Maybe (Map.Map String Value) -- ^ An object containing the key-value pairs referenced using liquid in your message.
  , sendemailSendUnderscoreat :: Maybe Int -- ^ A unix timestamp (seconds since epoch) determining when the message will be sent. The timestamp can be up to 90 days in the future. If this value is in the past, your message is sent immediately.
  , sendemailDisableUnderscoremessageUnderscoreretention :: Maybe Bool -- ^ If true, the message body is not retained in delivery history. Setting this value overrides the value set in the settings of your `transactional_message_id`.
  , sendemailSendUnderscoretoUnderscoreunsubscribed :: Maybe Bool -- ^ If false, your message is not sent to unsubscribed recipients. Setting this value overrides the value set in the settings of your `transactional_message_id`.
  , sendemailQueueUnderscoredraft :: Maybe Bool -- ^ If true, your transactional message is held as a draft in Customer.io and not sent directly to your audience. You must go to the Deliveries and Drafts page to send your message.
  , sendemailTo :: Text -- ^ The message recipient(s). Supports multiple addresses separated by commas. Your request can contain up to 15 total recipients between the `to` and `bcc` keys.  You can include a display or \"friendly\" name in \"to\" address, but we recommend that you use quotation marks around the friendly name to avoid potential issues with special characters, e.g. `\\\"Person\\\" <person@example.com>`.             
  , sendemailBcc :: Maybe Text -- ^ Blind copy message recipients. Supports multiple addresses separated by commas. Your request can contain up to 15 total recipients between the `to` and `bcc` keys.
  , sendemailFakeUnderscorebcc :: Maybe Bool -- ^ If true, rather than sending true copies to BCC addresses, Customer.io sends a copy of the message with the subject line containing the recipient address(es). 
  , sendemailReplyUnderscoreto :: Maybe Text -- ^ The address that recipients can reply to, if different from the `from` address.
  , sendemailPreheader :: Maybe Text -- ^ Also known as \"preview text\", this is the block block of text that users see next to, or underneath, the subject line in their inbox.
  , sendemailAttachments :: Maybe TransactionalSharedEmailObjectAttachments -- ^ 
  , sendemailHeaders :: Maybe (Map.Map String Text) -- ^ An object containing headers, where the key is the header name and the value is the header value. Header names and values must be strings and cannot contain any non-ASCII characters or empty spaces. Some headers are reserved and cannot be overwritten.
  , sendemailDisableUnderscorecssUnderscorepreprocessing :: Maybe Bool -- ^ Set to `true` to disable CSS preprocessing. This setting overrides the CSS preprocessing setting on the `transactional_message_id` as set in the user interface. Transactional emails have CSS preprocessing enabled by default.
  , sendemailTracked :: Maybe Bool -- ^ If true, Customer.io tracks opens and link clicks in your message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Sendemail where
  parseJSON = genericParseJSON optionsSendemail
instance ToJSON Sendemail where
  toJSON = genericToJSON optionsSendemail

optionsSendemail :: Options
optionsSendemail =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("sendemailTransactionalUnderscoremessageUnderscoreid", "transactional_message_id")
      , ("sendemailBody", "body")
      , ("sendemailBodyUnderscoreamp", "body_amp")
      , ("sendemailBodyUnderscoreplain", "body_plain")
      , ("sendemailSubject", "subject")
      , ("sendemailFrom", "from")
      , ("sendemailLanguage", "language")
      , ("sendemailIdentifiers", "identifiers")
      , ("sendemailMessageUnderscoredata", "message_data")
      , ("sendemailSendUnderscoreat", "send_at")
      , ("sendemailDisableUnderscoremessageUnderscoreretention", "disable_message_retention")
      , ("sendemailSendUnderscoretoUnderscoreunsubscribed", "send_to_unsubscribed")
      , ("sendemailQueueUnderscoredraft", "queue_draft")
      , ("sendemailTo", "to")
      , ("sendemailBcc", "bcc")
      , ("sendemailFakeUnderscorebcc", "fake_bcc")
      , ("sendemailReplyUnderscoreto", "reply_to")
      , ("sendemailPreheader", "preheader")
      , ("sendemailAttachments", "attachments")
      , ("sendemailHeaders", "headers")
      , ("sendemailDisableUnderscorecssUnderscorepreprocessing", "disable_css_preprocessing")
      , ("sendemailTracked", "tracked")
      ]


-- | Information about a sender.
data SenderIdentityObject = SenderIdentityObject
  { senderIdentityObjectId :: Maybe Int -- ^ The identifier of a sender.
  , senderIdentityObjectDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , senderIdentityObjectName :: Maybe Text -- ^ The name of the sender.
  , senderIdentityObjectEmail :: Maybe Text -- ^ The email address of the sender.
  , senderIdentityObjectAddress :: Maybe Text -- ^ The sender name and email address in the format `name <name@example.com>`.
  , senderIdentityObjectTemplateUnderscoretype :: Maybe Text -- ^ The type of sender.
  , senderIdentityObjectAutoUnderscoregenerated :: Maybe Bool -- ^ If true, the sender is automatically generated by Customer.io.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SenderIdentityObject where
  parseJSON = genericParseJSON optionsSenderIdentityObject
instance ToJSON SenderIdentityObject where
  toJSON = genericToJSON optionsSenderIdentityObject

optionsSenderIdentityObject :: Options
optionsSenderIdentityObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("senderIdentityObjectId", "id")
      , ("senderIdentityObjectDeduplicateUnderscoreid", "deduplicate_id")
      , ("senderIdentityObjectName", "name")
      , ("senderIdentityObjectEmail", "email")
      , ("senderIdentityObjectAddress", "address")
      , ("senderIdentityObjectTemplateUnderscoretype", "template_type")
      , ("senderIdentityObjectAutoUnderscoregenerated", "auto_generated")
      ]


-- | Determines the sending behavior for the action. &#x60;automatic&#x60; sends the action automatically when triggered; &#x60;draft&#x60; queues drafts when the action is triggered; or &#x60;off&#x60; to disable the action.
data SendingState = SendingState
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SendingState where
  parseJSON = genericParseJSON optionsSendingState
instance ToJSON SendingState where
  toJSON = genericToJSON optionsSendingState

optionsSendingState :: Options
optionsSendingState =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | The payload of a transactional push message.
data Sendpush = Sendpush
  { sendpushTransactionalUnderscoremessageUnderscoreid :: SendpushAllOfTransactionalMessageId -- ^ 
  , sendpushTo :: Text -- ^ The person's device(s) you want to send this push to. One of `all`, `last_used`, or a custom device token which belongs to the profile from the Identifiers block. Defaults to 'all'. This overrides To from the transactional template (referenced by `transactional_message_id`).
  , sendpushTitle :: Maybe Text -- ^ The title for your notification. This overrides the title of the transactional template (referenced by `transactional_message_id`).
  , sendpushMessage :: Maybe Text -- ^ The message body for your notification. This overrides the notification body of the transactional template (referenced by `transactional_message_id`).
  , sendpushImageUnderscoreurl :: Maybe Text -- ^ An image URL to show in the push. This overrides Image from the transactional template (referenced by `transactional_message_id`).
  , sendpushLink :: Maybe Text -- ^ A deep link to open when the push is tapped. This overrides Link from the transactional template (referenced by `transactional_message_id`).
  , sendpushSound :: Maybe Text -- ^ **For iOS Only**: your notification can alert users with the device's default notification sound or play no sound at all.  
  , sendpushCustomUnderscoredata :: Maybe Value -- ^ An optional list of key/value pairs to attach to the push payload. Due to a Firebase limitation we only support sending string key value pairs. This overrides Custom Data from the transactional template (referenced by `transactional_message_id`).
  , sendpushCustomUnderscoredevice :: Maybe SendpushAllOfCustomDevice -- ^ 
  , sendpushCustomUnderscorepayload :: Maybe SendpushAllOfCustomPayload -- ^ 
  , sendpushLanguage :: Maybe Text -- ^ Overrides language preferences for the person you want to send your transactional message to. Use one of our [supported two- or four-letter language codes](/localization-getting-started/#supported-languages).
  , sendpushIdentifiers :: SendpushAllOfIdentifiers -- ^ 
  , sendpushMessageUnderscoredata :: Maybe (Map.Map String Value) -- ^ An object containing the key-value pairs referenced using liquid in your message.
  , sendpushSendUnderscoreat :: Maybe Int -- ^ A unix timestamp (seconds since epoch) determining when the message will be sent. The timestamp can be up to 90 days in the future. If this value is in the past, your message is sent immediately.
  , sendpushDisableUnderscoremessageUnderscoreretention :: Maybe Bool -- ^ If true, the message body is not retained in delivery history. Setting this value overrides the value set in the settings of your `transactional_message_id`.
  , sendpushSendUnderscoretoUnderscoreunsubscribed :: Maybe Bool -- ^ If false, your message is not sent to unsubscribed recipients. Setting this value overrides the value set in the settings of your `transactional_message_id`.
  , sendpushQueueUnderscoredraft :: Maybe Bool -- ^ If true, your transactional message is held as a draft in Customer.io and not sent directly to your audience. You must go to the Deliveries and Drafts page to send your message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Sendpush where
  parseJSON = genericParseJSON optionsSendpush
instance ToJSON Sendpush where
  toJSON = genericToJSON optionsSendpush

optionsSendpush :: Options
optionsSendpush =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("sendpushTransactionalUnderscoremessageUnderscoreid", "transactional_message_id")
      , ("sendpushTo", "to")
      , ("sendpushTitle", "title")
      , ("sendpushMessage", "message")
      , ("sendpushImageUnderscoreurl", "image_url")
      , ("sendpushLink", "link")
      , ("sendpushSound", "sound")
      , ("sendpushCustomUnderscoredata", "custom_data")
      , ("sendpushCustomUnderscoredevice", "custom_device")
      , ("sendpushCustomUnderscorepayload", "custom_payload")
      , ("sendpushLanguage", "language")
      , ("sendpushIdentifiers", "identifiers")
      , ("sendpushMessageUnderscoredata", "message_data")
      , ("sendpushSendUnderscoreat", "send_at")
      , ("sendpushDisableUnderscoremessageUnderscoreretention", "disable_message_retention")
      , ("sendpushSendUnderscoretoUnderscoreunsubscribed", "send_to_unsubscribed")
      , ("sendpushQueueUnderscoredraft", "queue_draft")
      ]


-- | A device to perform an upsert operation at the time of send. The device will be added/updated on the profile from the Identifiers block.
data SendpushAllOfCustomDevice = SendpushAllOfCustomDevice
  { sendpushAllOfCustomDeviceToken :: Text -- ^ The device token.
  , sendpushAllOfCustomDeviceLastUnderscoreused :: Maybe Int -- ^ The `timestamp` when you last identified this device. If you don't pass a timestamp when you add or update a device, we use the time of the request itself. Our SDKs identify a device when a person launches their app.
  , sendpushAllOfCustomDevicePlatform :: Text -- ^ The device/messaging platform.
  , sendpushAllOfCustomDeviceAttributes :: Maybe AddDeviceRequestDeviceAllOfAttributes -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SendpushAllOfCustomDevice where
  parseJSON = genericParseJSON optionsSendpushAllOfCustomDevice
instance ToJSON SendpushAllOfCustomDevice where
  toJSON = genericToJSON optionsSendpushAllOfCustomDevice

optionsSendpushAllOfCustomDevice :: Options
optionsSendpushAllOfCustomDevice =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("sendpushAllOfCustomDeviceToken", "token")
      , ("sendpushAllOfCustomDeviceLastUnderscoreused", "last_used")
      , ("sendpushAllOfCustomDevicePlatform", "platform")
      , ("sendpushAllOfCustomDeviceAttributes", "attributes")
      ]


-- | An optional list of key/value pairs to attach to the push payload. Due to a Firebase limitation we only support sending string key value pairs. This overrides every other parameter, including any Custom Payload from the transactional template (referenced by &#x60;transactional_message_id&#x60;).
data SendpushAllOfCustomPayload = SendpushAllOfCustomPayload
  { sendpushAllOfCustomPayloadIos :: Maybe SendpushAllOfCustomPayloadIos -- ^ 
  , sendpushAllOfCustomPayloadAndroid :: Maybe TransactionalSharedPushObjectCustomPayloadAndroid -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SendpushAllOfCustomPayload where
  parseJSON = genericParseJSON optionsSendpushAllOfCustomPayload
instance ToJSON SendpushAllOfCustomPayload where
  toJSON = genericToJSON optionsSendpushAllOfCustomPayload

optionsSendpushAllOfCustomPayload :: Options
optionsSendpushAllOfCustomPayload =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("sendpushAllOfCustomPayloadIos", "ios")
      , ("sendpushAllOfCustomPayloadAndroid", "android")
      ]


-- | Your payload changes depending on whether you send to iOS devices through Google&#39;s Firebase Cloud Messaging (FCM) or Apple&#39;s Push Notification service (APNs).
data SendpushAllOfCustomPayloadIos = SendpushAllOfCustomPayloadIos
  { sendpushAllOfCustomPayloadIosMessage :: FCMMessage -- ^ 
  , sendpushAllOfCustomPayloadIosCIO :: Maybe APNSCIO -- ^ 
  , sendpushAllOfCustomPayloadIosAps :: Maybe FCMMessageApnsPayloadAps -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SendpushAllOfCustomPayloadIos where
  parseJSON = genericParseJSON optionsSendpushAllOfCustomPayloadIos
instance ToJSON SendpushAllOfCustomPayloadIos where
  toJSON = genericToJSON optionsSendpushAllOfCustomPayloadIos

optionsSendpushAllOfCustomPayloadIos :: Options
optionsSendpushAllOfCustomPayloadIos =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("sendpushAllOfCustomPayloadIosMessage", "message")
      , ("sendpushAllOfCustomPayloadIosCIO", "CIO")
      , ("sendpushAllOfCustomPayloadIosAps", "aps")
      ]


-- | Identifies the person represented by your transactional message by one of, and only one of, &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;.
data SendpushAllOfIdentifiers = SendpushAllOfIdentifiers
  { sendpushAllOfIdentifiersId :: Text -- ^ The identifier for the person represented by the transactional message. **NOTE**: If your workspace identifies people by email, use the `email` identifier instead. 
  , sendpushAllOfIdentifiersEmail :: Text -- ^ The identifier for the person represented by the transactional message. Use this option if your workspace identifies people by email rather than by `id`.
  , sendpushAllOfIdentifiersCioUnderscoreid :: Text -- ^ A unique, immutable identifier for a person, set by Customer.io when you add a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SendpushAllOfIdentifiers where
  parseJSON = genericParseJSON optionsSendpushAllOfIdentifiers
instance ToJSON SendpushAllOfIdentifiers where
  toJSON = genericToJSON optionsSendpushAllOfIdentifiers

optionsSendpushAllOfIdentifiers :: Options
optionsSendpushAllOfIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("sendpushAllOfIdentifiersId", "id")
      , ("sendpushAllOfIdentifiersEmail", "email")
      , ("sendpushAllOfIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | The transactional message template that you want to use for your message. You can call the template by its numerical ID or by the *Trigger Name* that you assigned the template (case insensitive).
data SendpushAllOfTransactionalMessageId = SendpushAllOfTransactionalMessageId
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SendpushAllOfTransactionalMessageId where
  parseJSON = genericParseJSON optionsSendpushAllOfTransactionalMessageId
instance ToJSON SendpushAllOfTransactionalMessageId where
  toJSON = genericToJSON optionsSendpushAllOfTransactionalMessageId

optionsSendpushAllOfTransactionalMessageId :: Options
optionsSendpushAllOfTransactionalMessageId =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data SimpleAudienceFilter = SimpleAudienceFilter
  { simpleAudienceFilterSegment :: Maybe Segment -- ^ 
  , simpleAudienceFilterAttribute :: Maybe Attribute -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SimpleAudienceFilter where
  parseJSON = genericParseJSON optionsSimpleAudienceFilter
instance ToJSON SimpleAudienceFilter where
  toJSON = genericToJSON optionsSimpleAudienceFilter

optionsSimpleAudienceFilter :: Options
optionsSimpleAudienceFilter =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("simpleAudienceFilterSegment", "segment")
      , ("simpleAudienceFilterAttribute", "attribute")
      ]


-- | 
data Slack = Slack
  { slackDeliveryUnderscoreid :: Text -- ^ The CIO-Delivery-ID from the notification that you want to associate the `event` with.
  , slackTimestamp :: Maybe Int -- ^ The unix timestamp when the event occurred.
  , slackMetric :: Text -- ^ The metric you want to report back to Customer.io.
  , slackHref :: Maybe Text -- ^ For `clicked` metrics, this is the link the recipient clicked.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Slack where
  parseJSON = genericParseJSON optionsSlack
instance ToJSON Slack where
  toJSON = genericToJSON optionsSlack

optionsSlack :: Options
optionsSlack =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("slackDeliveryUnderscoreid", "delivery_id")
      , ("slackTimestamp", "timestamp")
      , ("slackMetric", "metric")
      , ("slackHref", "href")
      ]


-- | Describes the slack events reported from Customer.io to a webhook.
data SlackEvents = SlackEvents
  { slackEventsSlackUnderscoreattempted :: Maybe Bool -- ^ Reports when a message could not be sent to the delivery provider will retry. Set to true to report this event type.
  , slackEventsSlackUnderscoreclicked :: Maybe Bool -- ^ Reports when a person clicks a tracked link in a message. Set to true to report this event type.
  , slackEventsSlackUnderscoreconverted :: Maybe Bool -- ^ Reports when a person matches a conversion goal attributed to a a message. Set to true to report this event type.
  , slackEventsSlackUnderscoredrafted :: Maybe Bool -- ^ Reports when a message draft is created. Set to true to report this event type.
  , slackEventsSlackUnderscorefailed :: Maybe Bool -- ^ Reports when a message couldn't be sent to the delivery provider. Set to true to report this event type.
  , slackEventsSlackUnderscoresent :: Maybe Bool -- ^ Reports when a message is sent from Customer.io to the delivery provider. Set to true to report this event type.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SlackEvents where
  parseJSON = genericParseJSON optionsSlackEvents
instance ToJSON SlackEvents where
  toJSON = genericToJSON optionsSlackEvents

optionsSlackEvents :: Options
optionsSlackEvents =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("slackEventsSlackUnderscoreattempted", "slack_attempted")
      , ("slackEventsSlackUnderscoreclicked", "slack_clicked")
      , ("slackEventsSlackUnderscoreconverted", "slack_converted")
      , ("slackEventsSlackUnderscoredrafted", "slack_drafted")
      , ("slackEventsSlackUnderscorefailed", "slack_failed")
      , ("slackEventsSlackUnderscoresent", "slack_sent")
      ]


-- | 
data SmsActionObject = SmsActionObject
  { smsActionObjectDeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , smsActionObjectName :: Maybe Text -- ^ The name of the action.
  , smsActionObjectLayout :: Maybe Text -- ^ The layout used for the action, if it exists.
  , smsActionObjectCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , smsActionObjectUpdated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , smsActionObjectBody :: Maybe Text -- ^ The body of your SMS message.
  , smsActionObjectLanguage :: Maybe Text -- ^ The language variant for your message. If you don't use our [localization feature](/localization), or this is the default message, this value is an empty string.
  , smsActionObjectType :: Maybe Text -- ^ For SMS/MMS messages, the `type` is always `twilio`.
  , smsActionObjectImageUnderscoreurl :: Maybe Text -- ^ The URL of the image in your SMS (MMS) message.
  , smsActionObjectSendingUnderscorestate :: Maybe Text -- ^ Determines the sending behavior for the action. `automatic` sends the action automatically when triggered; `draft` queues drafts when the action is triggered; or `off` to disable the action.
  , smsActionObjectRecipient :: Maybe Text -- ^ The recipient value. In general, your recipient is an attribute that you reference using liquid, like `{{customer.phone}}`, instead of a hard-coded value. If you set this field to a liquid statement like `{{customer.phone}}`, the field returns blank in `GET` requests because we populate the recipient from your liquid statement at send time.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SmsActionObject where
  parseJSON = genericParseJSON optionsSmsActionObject
instance ToJSON SmsActionObject where
  toJSON = genericToJSON optionsSmsActionObject

optionsSmsActionObject :: Options
optionsSmsActionObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("smsActionObjectDeduplicateUnderscoreid", "deduplicate_id")
      , ("smsActionObjectName", "name")
      , ("smsActionObjectLayout", "layout")
      , ("smsActionObjectCreated", "created")
      , ("smsActionObjectUpdated", "updated")
      , ("smsActionObjectBody", "body")
      , ("smsActionObjectLanguage", "language")
      , ("smsActionObjectType", "type")
      , ("smsActionObjectImageUnderscoreurl", "image_url")
      , ("smsActionObjectSendingUnderscorestate", "sending_state")
      , ("smsActionObjectRecipient", "recipient")
      ]


-- | Describes the SMS events reported from Customer.io to a webhook.
data SmsEvents = SmsEvents
  { smsEventsSmsUnderscoreattempted :: Maybe Bool -- ^ Reports when a push notification could not be sent to the delivery provider will retry. Set to true to report this event type.
  , smsEventsSmsUnderscorebounced :: Maybe Bool -- ^ Reports when the delivery provider is unable to deliver a message to the recipient. Set to true to report this event type.
  , smsEventsSmsUnderscoreclicked :: Maybe Bool -- ^ Reports when a person clicks a tracked link in a message. Set to true to report this event type.
  , smsEventsSmsUnderscoreconverted :: Maybe Bool -- ^ Reports when a person matches a conversion goal attributed to a a message. Set to true to report this event type.
  , smsEventsSmsUnderscoredelivered :: Maybe Bool -- ^ The delivery provider reports that the message is delivered. Set to true to report this event type.
  , smsEventsSmsUnderscoredrafted :: Maybe Bool -- ^ Reports when a message draft is created. Set to true to report this event type.
  , smsEventsSmsUnderscorefailed :: Maybe Bool -- ^ Reports when a message couldn't be sent to the delivery provider. Set to true to report this event type.
  , smsEventsSmsUnderscoresent :: Maybe Bool -- ^ Reports when a message is sent from Customer.io to the delivery provider. Set to true to report this event type.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SmsEvents where
  parseJSON = genericParseJSON optionsSmsEvents
instance ToJSON SmsEvents where
  toJSON = genericToJSON optionsSmsEvents

optionsSmsEvents :: Options
optionsSmsEvents =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("smsEventsSmsUnderscoreattempted", "sms_attempted")
      , ("smsEventsSmsUnderscorebounced", "sms_bounced")
      , ("smsEventsSmsUnderscoreclicked", "sms_clicked")
      , ("smsEventsSmsUnderscoreconverted", "sms_converted")
      , ("smsEventsSmsUnderscoredelivered", "sms_delivered")
      , ("smsEventsSmsUnderscoredrafted", "sms_drafted")
      , ("smsEventsSmsUnderscorefailed", "sms_failed")
      , ("smsEventsSmsUnderscoresent", "sms_sent")
      ]


-- | describes a piece of reusable content. You must provide a name for the snippet and the &#x60;value&#x60;—the content that appears in messages that use the snippet.
data Snippet = Snippet
  { snippetName :: Text -- ^ The name of the snippet, must be unique.
  , snippetValue :: Text -- ^ The contents of the snippet.
  , snippetUpdatedUnderscoreat :: Maybe Int -- ^ The last date-time the snippet was updated.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Snippet where
  parseJSON = genericParseJSON optionsSnippet
instance ToJSON Snippet where
  toJSON = genericToJSON optionsSnippet

optionsSnippet :: Options
optionsSnippet =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("snippetName", "name")
      , ("snippetValue", "value")
      , ("snippetUpdatedUnderscoreat", "updated_at")
      ]


-- | 
data StandardAnonymousEvent = StandardAnonymousEvent
  { standardAnonymousEventAnonymousUnderscoreid :: Maybe Text -- ^ An identifier for an anonymous event, like a cookie. If set as an attribute on a person, any events bearing the same anonymous value are associated with this person. This value must be unique and is not reusable.
  , standardAnonymousEventName :: Text -- ^ The name of the event. This is how you'll reference the event in campaigns or segments.
  , standardAnonymousEventId :: Maybe Text -- ^ An identifier used to deduplicate events. This value must be a [ULID](https://github.com/ulid/spec). If an event has the same value as an event we previously received, we won't show or process the duplicate. Note - our Python and Ruby libraries do not pass this id.
  , standardAnonymousEventType :: Maybe Text -- ^ Sets the event type. If your event isn't a `page` or `screen` type event, we automatically set this property to `event`.
  , standardAnonymousEventTimestamp :: Maybe Int -- ^ The unix timestamp when the event took place. If you don't provide this value, we use the date-time when we receive the event. 
  , standardAnonymousEventData :: Maybe StandardAnonymousEventData -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON StandardAnonymousEvent where
  parseJSON = genericParseJSON optionsStandardAnonymousEvent
instance ToJSON StandardAnonymousEvent where
  toJSON = genericToJSON optionsStandardAnonymousEvent

optionsStandardAnonymousEvent :: Options
optionsStandardAnonymousEvent =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("standardAnonymousEventAnonymousUnderscoreid", "anonymous_id")
      , ("standardAnonymousEventName", "name")
      , ("standardAnonymousEventId", "id")
      , ("standardAnonymousEventType", "type")
      , ("standardAnonymousEventTimestamp", "timestamp")
      , ("standardAnonymousEventData", "data")
      ]


-- | Additional information that you might want to reference in a message using liquid or use to set attributes on your customer (referenced by &#x60;customer_id&#x60;). You can include &#x60;from_address&#x60; and &#x60;reply_to&#x60;, but the event can only trigger a campaign if it is associated with a person within 72 hours of its timestamp.
newtype StandardAnonymousEventData = StandardAnonymousEventData { unStandardAnonymousEventData :: (Map.Map Text Value) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data StandardEvent = StandardEvent
  { standardEventName :: Text -- ^ The name of the event. This is how you'll reference the event in campaigns or segments.
  , standardEventId :: Maybe Text -- ^ An identifier used to deduplicate events. This value must be a [ULID](https://github.com/ulid/spec). If an event has the same value as an event we previously received, we won't show or process the duplicate. Note - our Python and Ruby libraries do not pass this id.
  , standardEventType :: Maybe Text -- ^ Sets the event type. If your event isn't a `page` or `screen` type event, we automatically set this property to `event`.
  , standardEventTimestamp :: Maybe Int -- ^ The unix timestamp when the event took place. If you don't provide this value, we use the date-time when we receive the event.  **NOTE**: Events with a timestamp in the past 72 hours can trigger campaigns. 
  , standardEventData :: Maybe StandardEventData -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON StandardEvent where
  parseJSON = genericParseJSON optionsStandardEvent
instance ToJSON StandardEvent where
  toJSON = genericToJSON optionsStandardEvent

optionsStandardEvent :: Options
optionsStandardEvent =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("standardEventName", "name")
      , ("standardEventId", "id")
      , ("standardEventType", "type")
      , ("standardEventTimestamp", "timestamp")
      , ("standardEventData", "data")
      ]


-- | Additional information that you might want to reference in a message using liquid or use to set attributes on your customer (referenced by &#x60;customer_id&#x60;).
newtype StandardEventData = StandardEventData { unStandardEventData :: (Map.Map Text Value) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | The status of the campaign.
data State = State
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON State where
  parseJSON = genericParseJSON optionsState
instance ToJSON State where
  toJSON = genericToJSON optionsState

optionsState :: Options
optionsState =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | The body of the request contains key-value pairs representing form fields; these values are mapped to attributes. Your request must contain one of—and only one of—&#x60;email&#x60; or &#x60;id&#x60; to identify a person (depending on the identifiers supported in your workspace). If the person who filled out your form does not already exist, the request creates them. If your request includes more than one identifier, you&#39;ll receive an error.  **NOTE**: If your form field is called something like &#x60;email_address&#x60;, you&#39;ll receive a &#x60;400&#x60;, but we&#39;ll still add your form on the **Data &amp; Integrations** &gt; **Integrations** &gt; **Forms** page. You can then re-map your &#x60;email_address&#x60; field to &#x60;email&#x60;, and your form will begin working normally.   Additional keys in the &#x60;data&#x60; object represent form fields from the form that a person submitted. By default, we map form fields in your request directly to attributes, e.g. if you have a form field called &#x60;first_name&#x60;, we map that field to the &#x60;first_name&#x60; attribute. However, if you added or edited this form on the *Data &amp; Integration* &gt; *Forms* page, you can re-map form fields to attributes. If you turned off a form field on the *Forms* page, you can still include it in your request, but it is not applied to the person your form identifies. 
data SubmitFormRequest = SubmitFormRequest
  { submitFormRequestData :: SubmitFormRequestData -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitFormRequest where
  parseJSON = genericParseJSON optionsSubmitFormRequest
instance ToJSON SubmitFormRequest where
  toJSON = genericToJSON optionsSubmitFormRequest

optionsSubmitFormRequest :: Options
optionsSubmitFormRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("submitFormRequestData", "data")
      ]


-- | Represents your form data. By default, we assume that form fields map directly to attributes (e.g. if your form field is called &#x60;name&#x60;, we assume it represents an attribute called \&quot;name\&quot;). However, you can re-map form fields to attributes on the **Forms** page in your workspace.  Values for form fields _must_ be formatted as strings. 
data SubmitFormRequestData = SubmitFormRequestData
  { submitFormRequestDataEmail :: Text -- ^ The email address of the customer.
  , submitFormRequestDataId :: Text -- ^ The ID of a customer profile, analogous to a \"person\" in the UI. If your workspace supports multiple identifiers (email and ID), this value can be null.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubmitFormRequestData where
  parseJSON = genericParseJSON optionsSubmitFormRequestData
instance ToJSON SubmitFormRequestData where
  toJSON = genericToJSON optionsSubmitFormRequestData

optionsSubmitFormRequestData :: Options
optionsSubmitFormRequestData =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("submitFormRequestDataEmail", "email")
      , ("submitFormRequestDataId", "id")
      ]


-- | Suppress a person&#39;s identifier(s) in Customer.io, so that you can&#39;t message a person or add their identifiers back to your workspace. This is separate from suppressions performed by your email provider.
data Suppress = Suppress
  { suppressType :: Text -- ^ The operation modifies a person in Customer.io
  , suppressIdentifiers :: IdentifyAllOfIdentifiers -- ^ 
  , suppressAction :: Text -- ^ Suppress a person's identifier(s) in Customer.io, so that you can't message a person or add their identifiers back to your workspace. This is separate from suppressions performed by your email provider.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Suppress where
  parseJSON = genericParseJSON optionsSuppress
instance ToJSON Suppress where
  toJSON = genericToJSON optionsSuppress

optionsSuppress :: Options
optionsSuppress =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("suppressType", "type")
      , ("suppressIdentifiers", "identifiers")
      , ("suppressAction", "action")
      ]


-- | Displays text in your message.
data TextWidget = TextWidget
  { textWidgetType :: Text -- ^ Defines the widget type.
  , textWidgetText :: Text -- ^ The text you want to display.
  , textWidgetStyle :: Maybe Text -- ^ The style of text you want to display. You can only set values here that are defined under [**Content** > **In-App Messages**](https://fly.customer.io/env/last/in-app-messages).
  , textWidgetColor :: Maybe Text -- ^ The color you want to use for this content. You can only set values here that are defined under [**Content** > **In-App Messages**](https://fly.customer.io/env/last/in-app-messages).
  , textWidgetTextAlign :: Maybe Text -- ^ How you want to align this text.
  , textWidgetMaxLines :: Maybe Int -- ^ The maximum lines of text you want to display. Text over this limit is controlled by the `overflow` property. If unset, the message displays an unlimited number of lines.
  , textWidgetOverflow :: Maybe Text -- ^ Determines how to handle text that overflows the `maxLines` limit (if set). By default, we cut off overflowing text with ellipsis (`...`).
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TextWidget where
  parseJSON = genericParseJSON optionsTextWidget
instance ToJSON TextWidget where
  toJSON = genericToJSON optionsTextWidget

optionsTextWidget :: Options
optionsTextWidget =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("textWidgetType", "type")
      , ("textWidgetText", "text")
      , ("textWidgetStyle", "style")
      , ("textWidgetColor", "color")
      , ("textWidgetTextAlign", "textAlign")
      , ("textWidgetMaxLines", "maxLines")
      , ("textWidgetOverflow", "overflow")
      ]


-- | An event attributed to an unknown person. If you provide an &#x60;anonymous_id&#x60; with the event, you can associate the event with a person later (using the anonymous ID).
data TrackAnonymousRequest = TrackAnonymousRequest
  { trackAnonymousRequestAnonymousUnderscoreid :: Maybe Text -- ^ An identifier for an anonymous event, like a cookie. If set as an attribute on a person, any events bearing the same anonymous value are associated with this person. This value must be unique and is not reusable.
  , trackAnonymousRequestName :: Text -- ^ The name of the event. In general, this should be the name of the screen or deep link path that a person viewed, making it easy to segment your audience or trigger campaigns using this event. Make sure you trim leading and trailing spaces from this field.
  , trackAnonymousRequestId :: Maybe Text -- ^ An identifier used to deduplicate events. This value must be a [ULID](https://github.com/ulid/spec). If an event has the same value as an event we previously received, we won't show or process the duplicate. Note - our Python and Ruby libraries do not pass this id.
  , trackAnonymousRequestType :: Text -- ^ Sets the event type. If your event isn't a `page` or `screen` type event, we automatically set this property to `event`.
  , trackAnonymousRequestTimestamp :: Maybe Int -- ^ The unix timestamp when the event took place. If you don't provide this value, we use the date-time when we receive the event. 
  , trackAnonymousRequestData :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on your customer (referenced by `customer_id`).
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TrackAnonymousRequest where
  parseJSON = genericParseJSON optionsTrackAnonymousRequest
instance ToJSON TrackAnonymousRequest where
  toJSON = genericToJSON optionsTrackAnonymousRequest

optionsTrackAnonymousRequest :: Options
optionsTrackAnonymousRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("trackAnonymousRequestAnonymousUnderscoreid", "anonymous_id")
      , ("trackAnonymousRequestName", "name")
      , ("trackAnonymousRequestId", "id")
      , ("trackAnonymousRequestType", "type")
      , ("trackAnonymousRequestTimestamp", "timestamp")
      , ("trackAnonymousRequestData", "data")
      ]


-- | 
data TrackIdentifierParameter = TrackIdentifierParameter
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TrackIdentifierParameter where
  parseJSON = genericParseJSON optionsTrackIdentifierParameter
instance ToJSON TrackIdentifierParameter where
  toJSON = genericToJSON optionsTrackIdentifierParameter

optionsTrackIdentifierParameter :: Options
optionsTrackIdentifierParameter =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | The base properties shared across multiple metric types.
data TrackMetrics = TrackMetrics
  { trackMetricsDeliveryUnderscoreid :: Text -- ^ The CIO-Delivery-ID from the notification that you want to associate the `event` with.
  , trackMetricsTimestamp :: Maybe Int -- ^ The unix timestamp when the event occurred.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TrackMetrics where
  parseJSON = genericParseJSON optionsTrackMetrics
instance ToJSON TrackMetrics where
  toJSON = genericToJSON optionsTrackMetrics

optionsTrackMetrics :: Options
optionsTrackMetrics =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("trackMetricsDeliveryUnderscoreid", "delivery_id")
      , ("trackMetricsTimestamp", "timestamp")
      ]


-- | 
data TrackRequest = TrackRequest
  { trackRequestName :: Text -- ^ The name of the event. In general, this should be the name of the screen or deep link path that a person viewed, making it easy to segment your audience or trigger campaigns from these events. Make sure you trim leading and trailing spaces from this field.
  , trackRequestId :: Maybe Text -- ^ An identifier used to deduplicate events. This value must be a [ULID](https://github.com/ulid/spec). If an event has the same value as an event we previously received, we won't show or process the duplicate. Note - our Python and Ruby libraries do not pass this id.
  , trackRequestType :: Text -- ^ Sets the event type. If your event isn't a `page` or `screen` type event, we automatically set this property to `event`.
  , trackRequestTimestamp :: Maybe Int -- ^ The unix timestamp when the event took place. If you don't provide this value, we use the date-time when we receive the event. 
  , trackRequestData :: Maybe (Map.Map String Value) -- ^ Additional information that you might want to reference in a message using liquid or use to set attributes on your customer (referenced by `customer_id`).
  , trackRequestAnonymousUnderscoreid :: Text -- ^ An identifier for an anonymous event, like a cookie. If set as an attribute on a person, any events bearing the same anonymous value are associated with this person. This value must be unique and is not reusable.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TrackRequest where
  parseJSON = genericParseJSON optionsTrackRequest
instance ToJSON TrackRequest where
  toJSON = genericToJSON optionsTrackRequest

optionsTrackRequest :: Options
optionsTrackRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("trackRequestName", "name")
      , ("trackRequestId", "id")
      , ("trackRequestType", "type")
      , ("trackRequestTimestamp", "timestamp")
      , ("trackRequestData", "data")
      , ("trackRequestAnonymousUnderscoreid", "anonymous_id")
      ]


-- | 
data TransactionalActionObject = TransactionalActionObject
  { transactionalActionObjectId :: Maybe Int -- ^ The identifier for an action.
  , transactionalActionObjectName :: Maybe Text -- ^ The name of the transactional message.
  , transactionalActionObjectCreated :: Maybe Int -- ^ The date time when the referenced ID was created.
  , transactionalActionObjectUpdated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , transactionalActionObjectBody :: Maybe Text -- ^ The body of the transactional message. You cannot modify the body if you created it with our drag-and-drop editor.
  , transactionalActionObjectLanguage :: Maybe Text -- ^ The language variant for your message. If you don't use our [localization feature](/localization), or this is the default message, this value is an empty string.
  , transactionalActionObjectType :: Maybe Text -- ^ The type of message.
  , transactionalActionObjectFrom :: Maybe Text -- ^ The address that the message is from, relevant if the action `type` is `email`.
  , transactionalActionObjectFromUnderscoreid :: Maybe Int -- ^ The identifier of the `from` address, commonly known as the \"sender\". You can [list your sender identities](#operation/listSenders) to match the ID to a specific address.
  , transactionalActionObjectReplyUnderscoreto :: Maybe Text -- ^ The address that receives replies for the message, if applicable.
  , transactionalActionObjectReplyUnderscoretoUnderscoreid :: Maybe Int -- ^ The identifier for the `reply_to` address, if applicable. You can [list your sender identities](#operation/listSenders) to match the ID to a specific address.
  , transactionalActionObjectPreprocessor :: Maybe Text -- ^ By default, we process CSS before emails leave Customer.io using Premailer. If your message included CSS and pre-processing is not disabled, this key indicates the pre-processor.
  , transactionalActionObjectRecipient :: Maybe Text -- ^ The recipient address for an action.
  , transactionalActionObjectSubject :: Maybe Text -- ^ The subject line for an `email` action.
  , transactionalActionObjectBcc :: Maybe Text -- ^ The blind-copy address(es) for this action.
  , transactionalActionObjectFakeUnderscorebcc :: Maybe Bool -- ^ If true, rather than sending true copies to BCC addresses, Customer.io sends a copy of the message with the subject line containing the recipient address(es). 
  , transactionalActionObjectPreheaderUnderscoretext :: Maybe Text -- ^ Also known as \"preview text\", this specifies the small block of text shown in an end-user's email inbox, next to, or underneath, the subject line.
  , transactionalActionObjectHeaders :: Maybe (Map.Map String Text) -- ^ An object containing headers, where the key is the header name and the value is the header value. Header names and values must be strings and cannot contain any non-ASCII characters or empty spaces. Some headers are reserved and cannot be overwritten.
  , transactionalActionObjectBodyUnderscoreamp :: Maybe Text -- ^ If your message is an email, this is the AMP-enabled body of your message. If your recipient's email client doesn't support AMP, the `body` represents your fallback message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalActionObject where
  parseJSON = genericParseJSON optionsTransactionalActionObject
instance ToJSON TransactionalActionObject where
  toJSON = genericToJSON optionsTransactionalActionObject

optionsTransactionalActionObject :: Options
optionsTransactionalActionObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalActionObjectId", "id")
      , ("transactionalActionObjectName", "name")
      , ("transactionalActionObjectCreated", "created")
      , ("transactionalActionObjectUpdated", "updated")
      , ("transactionalActionObjectBody", "body")
      , ("transactionalActionObjectLanguage", "language")
      , ("transactionalActionObjectType", "type")
      , ("transactionalActionObjectFrom", "from")
      , ("transactionalActionObjectFromUnderscoreid", "from_id")
      , ("transactionalActionObjectReplyUnderscoreto", "reply_to")
      , ("transactionalActionObjectReplyUnderscoretoUnderscoreid", "reply_to_id")
      , ("transactionalActionObjectPreprocessor", "preprocessor")
      , ("transactionalActionObjectRecipient", "recipient")
      , ("transactionalActionObjectSubject", "subject")
      , ("transactionalActionObjectBcc", "bcc")
      , ("transactionalActionObjectFakeUnderscorebcc", "fake_bcc")
      , ("transactionalActionObjectPreheaderUnderscoretext", "preheader_text")
      , ("transactionalActionObjectHeaders", "headers")
      , ("transactionalActionObjectBodyUnderscoreamp", "body_amp")
      ]


-- | Contains information about a transactional message.
data TransactionalObject = TransactionalObject
  { transactionalObjectId :: Maybe Int -- ^ The identifier Customer.io assigned to the transactional message
  , transactionalObjectName :: Maybe Text -- ^ The name you set for the transactional message.
  , transactionalObjectDescription :: Maybe Text -- ^ A description of the transactional message.
  , transactionalObjectSendUnderscoretoUnderscoreunsubscribed :: Maybe Bool -- ^ If true, people with an `unsubscribed` attribute set to `true` can trigger the message.
  , transactionalObjectLinkUnderscoretracking :: Maybe Bool -- ^ If true, link tracking is enabled for this message.
  , transactionalObjectOpenUnderscoretracking :: Maybe Bool -- ^ If true, open-tracking is enabled for this message.
  , transactionalObjectHideUnderscoremessageUnderscorebody :: Maybe Bool -- ^ If true, message contents are not retained in delivery history—you cannot recall the exact contents of the message.
  , transactionalObjectQueueUnderscoredrafts :: Maybe Bool -- ^ If true, messages do not send automatically, and queue as drafts instead. You must send drafts through the *Deliveries & Drafts* page in the user interface.
  , transactionalObjectCreatedUnderscoreat :: Maybe Int -- ^ The date time when the referenced ID was created.
  , transactionalObjectUpdatedUnderscoreat :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalObject where
  parseJSON = genericParseJSON optionsTransactionalObject
instance ToJSON TransactionalObject where
  toJSON = genericToJSON optionsTransactionalObject

optionsTransactionalObject :: Options
optionsTransactionalObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalObjectId", "id")
      , ("transactionalObjectName", "name")
      , ("transactionalObjectDescription", "description")
      , ("transactionalObjectSendUnderscoretoUnderscoreunsubscribed", "send_to_unsubscribed")
      , ("transactionalObjectLinkUnderscoretracking", "link_tracking")
      , ("transactionalObjectOpenUnderscoretracking", "open_tracking")
      , ("transactionalObjectHideUnderscoremessageUnderscorebody", "hide_message_body")
      , ("transactionalObjectQueueUnderscoredrafts", "queue_drafts")
      , ("transactionalObjectCreatedUnderscoreat", "created_at")
      , ("transactionalObjectUpdatedUnderscoreat", "updated_at")
      ]


-- | 
data TransactionalSharedEmailObject = TransactionalSharedEmailObject
  { transactionalSharedEmailObjectTo :: Maybe Text -- ^ The message recipient(s). Supports multiple addresses separated by commas. Your request can contain up to 15 total recipients between the `to` and `bcc` keys.  You can include a display or \"friendly\" name in \"to\" address, but we recommend that you use quotation marks around the friendly name to avoid potential issues with special characters, e.g. `\\\"Person\\\" <person@example.com>`.             
  , transactionalSharedEmailObjectBcc :: Maybe Text -- ^ Blind copy message recipients. Supports multiple addresses separated by commas. Your request can contain up to 15 total recipients between the `to` and `bcc` keys.
  , transactionalSharedEmailObjectFakeUnderscorebcc :: Maybe Bool -- ^ If true, rather than sending true copies to BCC addresses, Customer.io sends a copy of the message with the subject line containing the recipient address(es). 
  , transactionalSharedEmailObjectReplyUnderscoreto :: Maybe Text -- ^ The address that recipients can reply to, if different from the `from` address.
  , transactionalSharedEmailObjectPreheader :: Maybe Text -- ^ Also known as \"preview text\", this is the block block of text that users see next to, or underneath, the subject line in their inbox.
  , transactionalSharedEmailObjectBodyUnderscoreplain :: Maybe Text -- ^ By default, we generate a plaintext version of your message body for each delivery. Use this key to override the default plain text body.
  , transactionalSharedEmailObjectAttachments :: Maybe TransactionalSharedEmailObjectAttachments -- ^ 
  , transactionalSharedEmailObjectHeaders :: Maybe (Map.Map String Text) -- ^ An object containing headers, where the key is the header name and the value is the header value. Header names and values must be strings and cannot contain any non-ASCII characters or empty spaces. Some headers are reserved and cannot be overwritten.
  , transactionalSharedEmailObjectDisableUnderscorecssUnderscorepreprocessing :: Maybe Bool -- ^ Set to `true` to disable CSS preprocessing. This setting overrides the CSS preprocessing setting on the `transactional_message_id` as set in the user interface. Transactional emails have CSS preprocessing enabled by default.
  , transactionalSharedEmailObjectTracked :: Maybe Bool -- ^ If true, Customer.io tracks opens and link clicks in your message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalSharedEmailObject where
  parseJSON = genericParseJSON optionsTransactionalSharedEmailObject
instance ToJSON TransactionalSharedEmailObject where
  toJSON = genericToJSON optionsTransactionalSharedEmailObject

optionsTransactionalSharedEmailObject :: Options
optionsTransactionalSharedEmailObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalSharedEmailObjectTo", "to")
      , ("transactionalSharedEmailObjectBcc", "bcc")
      , ("transactionalSharedEmailObjectFakeUnderscorebcc", "fake_bcc")
      , ("transactionalSharedEmailObjectReplyUnderscoreto", "reply_to")
      , ("transactionalSharedEmailObjectPreheader", "preheader")
      , ("transactionalSharedEmailObjectBodyUnderscoreplain", "body_plain")
      , ("transactionalSharedEmailObjectAttachments", "attachments")
      , ("transactionalSharedEmailObjectHeaders", "headers")
      , ("transactionalSharedEmailObjectDisableUnderscorecssUnderscorepreprocessing", "disable_css_preprocessing")
      , ("transactionalSharedEmailObjectTracked", "tracked")
      ]


-- | A dictionary of attachments where the filename is the key and the value is the base64-encoded contents. The filename must include the extension (i.e. &#x60;name.csv&#x60;). The total size of all attachments must be less than 2 MB.
data TransactionalSharedEmailObjectAttachments = TransactionalSharedEmailObjectAttachments
  { transactionalSharedEmailObjectAttachmentsLessThanfileDashnameGreaterThan :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalSharedEmailObjectAttachments where
  parseJSON = genericParseJSON optionsTransactionalSharedEmailObjectAttachments
instance ToJSON TransactionalSharedEmailObjectAttachments where
  toJSON = genericToJSON optionsTransactionalSharedEmailObjectAttachments

optionsTransactionalSharedEmailObjectAttachments :: Options
optionsTransactionalSharedEmailObjectAttachments =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalSharedEmailObjectAttachmentsLessThanfileDashnameGreaterThan", "<file-name>")
      ]


-- | 
data TransactionalSharedObject = TransactionalSharedObject
  { transactionalSharedObjectIdentifiers :: TransactionalSharedObjectIdentifiers -- ^ 
  , transactionalSharedObjectMessageUnderscoredata :: Maybe (Map.Map String Value) -- ^ An object containing the key-value pairs referenced using liquid in your message.
  , transactionalSharedObjectSendUnderscoreat :: Maybe Int -- ^ A unix timestamp (seconds since epoch) determining when the message will be sent. The timestamp can be up to 90 days in the future. If this value is in the past, your message is sent immediately.
  , transactionalSharedObjectDisableUnderscoremessageUnderscoreretention :: Maybe Bool -- ^ If true, the message body is not retained in delivery history. Setting this value overrides the value set in the settings of your `transactional_message_id`.
  , transactionalSharedObjectSendUnderscoretoUnderscoreunsubscribed :: Maybe Bool -- ^ If false, your message is not sent to unsubscribed recipients. Setting this value overrides the value set in the settings of your `transactional_message_id`.
  , transactionalSharedObjectQueueUnderscoredraft :: Maybe Bool -- ^ If true, your transactional message is held as a draft in Customer.io and not sent directly to your audience. You must go to the Deliveries and Drafts page to send your message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalSharedObject where
  parseJSON = genericParseJSON optionsTransactionalSharedObject
instance ToJSON TransactionalSharedObject where
  toJSON = genericToJSON optionsTransactionalSharedObject

optionsTransactionalSharedObject :: Options
optionsTransactionalSharedObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalSharedObjectIdentifiers", "identifiers")
      , ("transactionalSharedObjectMessageUnderscoredata", "message_data")
      , ("transactionalSharedObjectSendUnderscoreat", "send_at")
      , ("transactionalSharedObjectDisableUnderscoremessageUnderscoreretention", "disable_message_retention")
      , ("transactionalSharedObjectSendUnderscoretoUnderscoreunsubscribed", "send_to_unsubscribed")
      , ("transactionalSharedObjectQueueUnderscoredraft", "queue_draft")
      ]


-- | Identifies the person represented by your transactional message by one of, and only one of, &#x60;id&#x60;, &#x60;email&#x60;, or &#x60;cio_id&#x60;.
data TransactionalSharedObjectIdentifiers = TransactionalSharedObjectIdentifiers
  { transactionalSharedObjectIdentifiersId :: Text -- ^ The identifier for the person represented by the transactional message. **NOTE**: If your workspace identifies people by email, use the `email` identifier instead. 
  , transactionalSharedObjectIdentifiersEmail :: Text -- ^ The identifier for the person represented by the transactional message. Use this option if your workspace identifies people by email rather than by `id`.
  , transactionalSharedObjectIdentifiersCioUnderscoreid :: Text -- ^ A unique, immutable identifier for a person, set by Customer.io when you add a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalSharedObjectIdentifiers where
  parseJSON = genericParseJSON optionsTransactionalSharedObjectIdentifiers
instance ToJSON TransactionalSharedObjectIdentifiers where
  toJSON = genericToJSON optionsTransactionalSharedObjectIdentifiers

optionsTransactionalSharedObjectIdentifiers :: Options
optionsTransactionalSharedObjectIdentifiers =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalSharedObjectIdentifiersId", "id")
      , ("transactionalSharedObjectIdentifiersEmail", "email")
      , ("transactionalSharedObjectIdentifiersCioUnderscoreid", "cio_id")
      ]


-- | 
data TransactionalSharedPushObject = TransactionalSharedPushObject
  { transactionalSharedPushObjectTo :: Maybe Text -- ^ The person's device(s) you want to send this push to. One of `all`, `last_used`, or a custom device token which belongs to the profile from the Identifiers block. Defaults to 'all'. This overrides To from the transactional template (referenced by `transactional_message_id`).
  , transactionalSharedPushObjectTitle :: Maybe Text -- ^ The title for your notification. This overrides the title of the transactional template (referenced by `transactional_message_id`).
  , transactionalSharedPushObjectMessage :: Maybe Text -- ^ The message body for your notification. This overrides the notification body of the transactional template (referenced by `transactional_message_id`).
  , transactionalSharedPushObjectImageUnderscoreurl :: Maybe Text -- ^ An image URL to show in the push. This overrides Image from the transactional template (referenced by `transactional_message_id`).
  , transactionalSharedPushObjectLink :: Maybe Text -- ^ A deep link to open when the push is tapped. This overrides Link from the transactional template (referenced by `transactional_message_id`).
  , transactionalSharedPushObjectSound :: Maybe Text -- ^ **For iOS Only**: your notification can alert users with the device's default notification sound or play no sound at all.  
  , transactionalSharedPushObjectCustomUnderscoredata :: Maybe Value -- ^ An optional list of key/value pairs to attach to the push payload. Due to a Firebase limitation we only support sending string key value pairs. This overrides Custom Data from the transactional template (referenced by `transactional_message_id`).
  , transactionalSharedPushObjectCustomUnderscoredevice :: Maybe TransactionalSharedPushObjectCustomDevice -- ^ 
  , transactionalSharedPushObjectCustomUnderscorepayload :: Maybe TransactionalSharedPushObjectCustomPayload -- ^ 
  , transactionalSharedPushObjectLanguage :: Maybe Text -- ^ Overrides language preferences for the person you want to send your transactional message to. Use one of our [supported two- or four-letter language codes](/localization-getting-started/#supported-languages).
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalSharedPushObject where
  parseJSON = genericParseJSON optionsTransactionalSharedPushObject
instance ToJSON TransactionalSharedPushObject where
  toJSON = genericToJSON optionsTransactionalSharedPushObject

optionsTransactionalSharedPushObject :: Options
optionsTransactionalSharedPushObject =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalSharedPushObjectTo", "to")
      , ("transactionalSharedPushObjectTitle", "title")
      , ("transactionalSharedPushObjectMessage", "message")
      , ("transactionalSharedPushObjectImageUnderscoreurl", "image_url")
      , ("transactionalSharedPushObjectLink", "link")
      , ("transactionalSharedPushObjectSound", "sound")
      , ("transactionalSharedPushObjectCustomUnderscoredata", "custom_data")
      , ("transactionalSharedPushObjectCustomUnderscoredevice", "custom_device")
      , ("transactionalSharedPushObjectCustomUnderscorepayload", "custom_payload")
      , ("transactionalSharedPushObjectLanguage", "language")
      ]


-- | A device to perform an upsert operation at the time of send. The device will be added/updated on the profile from the Identifiers block.
data TransactionalSharedPushObjectCustomDevice = TransactionalSharedPushObjectCustomDevice
  { transactionalSharedPushObjectCustomDeviceToken :: Text -- ^ The device token.
  , transactionalSharedPushObjectCustomDeviceLastUnderscoreused :: Maybe Int -- ^ The `timestamp` when you last identified this device. If you don't pass a timestamp when you add or update a device, we use the time of the request itself. Our SDKs identify a device when a person launches their app.
  , transactionalSharedPushObjectCustomDevicePlatform :: Text -- ^ The device/messaging platform.
  , transactionalSharedPushObjectCustomDeviceAttributes :: Maybe AddDeviceRequestDeviceAllOfAttributes -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalSharedPushObjectCustomDevice where
  parseJSON = genericParseJSON optionsTransactionalSharedPushObjectCustomDevice
instance ToJSON TransactionalSharedPushObjectCustomDevice where
  toJSON = genericToJSON optionsTransactionalSharedPushObjectCustomDevice

optionsTransactionalSharedPushObjectCustomDevice :: Options
optionsTransactionalSharedPushObjectCustomDevice =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalSharedPushObjectCustomDeviceToken", "token")
      , ("transactionalSharedPushObjectCustomDeviceLastUnderscoreused", "last_used")
      , ("transactionalSharedPushObjectCustomDevicePlatform", "platform")
      , ("transactionalSharedPushObjectCustomDeviceAttributes", "attributes")
      ]


-- | An optional list of key/value pairs to attach to the push payload. Due to a Firebase limitation we only support sending string key value pairs. This overrides every other parameter, including any Custom Payload from the transactional template (referenced by &#x60;transactional_message_id&#x60;).
data TransactionalSharedPushObjectCustomPayload = TransactionalSharedPushObjectCustomPayload
  { transactionalSharedPushObjectCustomPayloadIos :: Maybe TransactionalSharedPushObjectCustomPayloadIos -- ^ 
  , transactionalSharedPushObjectCustomPayloadAndroid :: Maybe TransactionalSharedPushObjectCustomPayloadAndroid -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalSharedPushObjectCustomPayload where
  parseJSON = genericParseJSON optionsTransactionalSharedPushObjectCustomPayload
instance ToJSON TransactionalSharedPushObjectCustomPayload where
  toJSON = genericToJSON optionsTransactionalSharedPushObjectCustomPayload

optionsTransactionalSharedPushObjectCustomPayload :: Options
optionsTransactionalSharedPushObjectCustomPayload =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalSharedPushObjectCustomPayloadIos", "ios")
      , ("transactionalSharedPushObjectCustomPayloadAndroid", "android")
      ]


-- | A custom push payload for Android devices.
data TransactionalSharedPushObjectCustomPayloadAndroid = TransactionalSharedPushObjectCustomPayloadAndroid
  { transactionalSharedPushObjectCustomPayloadAndroidMessage :: TransactionalSharedPushObjectCustomPayloadAndroidMessage -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalSharedPushObjectCustomPayloadAndroid where
  parseJSON = genericParseJSON optionsTransactionalSharedPushObjectCustomPayloadAndroid
instance ToJSON TransactionalSharedPushObjectCustomPayloadAndroid where
  toJSON = genericToJSON optionsTransactionalSharedPushObjectCustomPayloadAndroid

optionsTransactionalSharedPushObjectCustomPayloadAndroid :: Options
optionsTransactionalSharedPushObjectCustomPayloadAndroid =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalSharedPushObjectCustomPayloadAndroidMessage", "message")
      ]


-- | The parent object for Android custom push payloads.
data TransactionalSharedPushObjectCustomPayloadAndroidMessage = TransactionalSharedPushObjectCustomPayloadAndroidMessage
  { transactionalSharedPushObjectCustomPayloadAndroidMessageNotification :: Maybe TransactionalSharedPushObjectCustomPayloadAndroidMessageNotification -- ^ 
  , transactionalSharedPushObjectCustomPayloadAndroidMessageData :: Maybe (Map.Map String Text) -- ^ Contains key-value pairs that your app interprets.
  , transactionalSharedPushObjectCustomPayloadAndroidMessageAndroid :: Maybe TransactionalSharedPushObjectCustomPayloadAndroidMessageAndroid -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalSharedPushObjectCustomPayloadAndroidMessage where
  parseJSON = genericParseJSON optionsTransactionalSharedPushObjectCustomPayloadAndroidMessage
instance ToJSON TransactionalSharedPushObjectCustomPayloadAndroidMessage where
  toJSON = genericToJSON optionsTransactionalSharedPushObjectCustomPayloadAndroidMessage

optionsTransactionalSharedPushObjectCustomPayloadAndroidMessage :: Options
optionsTransactionalSharedPushObjectCustomPayloadAndroidMessage =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalSharedPushObjectCustomPayloadAndroidMessageNotification", "notification")
      , ("transactionalSharedPushObjectCustomPayloadAndroidMessageData", "data")
      , ("transactionalSharedPushObjectCustomPayloadAndroidMessageAndroid", "android")
      ]


-- | Contains custom push options for your notification.
data TransactionalSharedPushObjectCustomPayloadAndroidMessageAndroid = TransactionalSharedPushObjectCustomPayloadAndroidMessageAndroid
  { transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotification :: Maybe TransactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotification -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalSharedPushObjectCustomPayloadAndroidMessageAndroid where
  parseJSON = genericParseJSON optionsTransactionalSharedPushObjectCustomPayloadAndroidMessageAndroid
instance ToJSON TransactionalSharedPushObjectCustomPayloadAndroidMessageAndroid where
  toJSON = genericToJSON optionsTransactionalSharedPushObjectCustomPayloadAndroidMessageAndroid

optionsTransactionalSharedPushObjectCustomPayloadAndroidMessageAndroid :: Options
optionsTransactionalSharedPushObjectCustomPayloadAndroidMessageAndroid =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotification", "notification")
      ]


-- | Properties supported specifically by Android on FCM.
data TransactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotification = TransactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotification
  { transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationIcon :: Maybe Text -- ^ Sets the notification icon to `myicon` for drawable resource `myicon`. If you don't send this key, FCM displays the launcher icon from your app manifest.
  , transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationSound :: Maybe Text -- ^ The sound that plays when the device receives the notification. Supports `\"default\"` or the filename of a sound resource bundled in your app. Sound files must reside in `/res/raw/`.
  , transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationTag :: Maybe Text -- ^ Identifier to replace existing notifications in the notification drawer. If empty, each request creates a new notification.  If you specify a tag, and a notification with the same tag is already being shown, the new notification replaces the existing one in the notification drawer.  
  , transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationColor :: Maybe Text -- ^ The notification's icon color in `#rrggbb` format.
  , transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationClickUnderscoreaction :: Maybe Text -- ^ The action that occurs when a user taps on the notification. Launches an activity with a matching intent filter when a person taps the notification.
  , transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationBodyUnderscorelocUnderscorekey :: Maybe Text -- ^ The key to the body string in the app's string resources that you want to use to localize the body text to the user's current localization. See [String Resources](https://developer.android.com/guide/topics/resources/string-resource/) for more information.
  , transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationBodyUnderscorelocUnderscorearg :: Maybe Text -- ^ Variable string values used in place of the format specifiers in `body_loc_key` to localize the body text to the user's current localization. See Formatting and Styling for more information.
  , transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationTitleUnderscorelocUnderscorekey :: Maybe Text -- ^ The key to the title string in the app's string resources that you want to use to localize the title text to the user's current localization. See [String Resources](https://developer.android.com/guide/topics/resources/string-resource/) for more information.
  , transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationTitleUnderscorelocUnderscorearg :: Maybe Text -- ^ Variable string values used in place of the format specifiers in `title_loc_key` to localize the title text to the user's current localization. See Formatting and Styling for more information.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotification where
  parseJSON = genericParseJSON optionsTransactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotification
instance ToJSON TransactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotification where
  toJSON = genericToJSON optionsTransactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotification

optionsTransactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotification :: Options
optionsTransactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotification =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationIcon", "icon")
      , ("transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationSound", "sound")
      , ("transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationTag", "tag")
      , ("transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationColor", "color")
      , ("transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationClickUnderscoreaction", "click_action")
      , ("transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationBodyUnderscorelocUnderscorekey", "body_loc_key")
      , ("transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationBodyUnderscorelocUnderscorearg", "body_loc_arg")
      , ("transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationTitleUnderscorelocUnderscorekey", "title_loc_key")
      , ("transactionalSharedPushObjectCustomPayloadAndroidMessageAndroidNotificationTitleUnderscorelocUnderscorearg", "title_loc_arg")
      ]


-- | Contains the push body and title.
data TransactionalSharedPushObjectCustomPayloadAndroidMessageNotification = TransactionalSharedPushObjectCustomPayloadAndroidMessageNotification
  { transactionalSharedPushObjectCustomPayloadAndroidMessageNotificationTitle :: Maybe Text -- ^ The title of your push notification.
  , transactionalSharedPushObjectCustomPayloadAndroidMessageNotificationBody :: Maybe Text -- ^ The body of your push notification.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalSharedPushObjectCustomPayloadAndroidMessageNotification where
  parseJSON = genericParseJSON optionsTransactionalSharedPushObjectCustomPayloadAndroidMessageNotification
instance ToJSON TransactionalSharedPushObjectCustomPayloadAndroidMessageNotification where
  toJSON = genericToJSON optionsTransactionalSharedPushObjectCustomPayloadAndroidMessageNotification

optionsTransactionalSharedPushObjectCustomPayloadAndroidMessageNotification :: Options
optionsTransactionalSharedPushObjectCustomPayloadAndroidMessageNotification =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalSharedPushObjectCustomPayloadAndroidMessageNotificationTitle", "title")
      , ("transactionalSharedPushObjectCustomPayloadAndroidMessageNotificationBody", "body")
      ]


-- | Your payload changes depending on whether you send to iOS devices through Google&#39;s Firebase Cloud Messaging (FCM) or Apple&#39;s Push Notification service (APNs).
data TransactionalSharedPushObjectCustomPayloadIos = TransactionalSharedPushObjectCustomPayloadIos
  { transactionalSharedPushObjectCustomPayloadIosMessage :: FCMMessage -- ^ 
  , transactionalSharedPushObjectCustomPayloadIosCIO :: Maybe APNSCIO -- ^ 
  , transactionalSharedPushObjectCustomPayloadIosAps :: Maybe FCMMessageApnsPayloadAps -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionalSharedPushObjectCustomPayloadIos where
  parseJSON = genericParseJSON optionsTransactionalSharedPushObjectCustomPayloadIos
instance ToJSON TransactionalSharedPushObjectCustomPayloadIos where
  toJSON = genericToJSON optionsTransactionalSharedPushObjectCustomPayloadIos

optionsTransactionalSharedPushObjectCustomPayloadIos :: Options
optionsTransactionalSharedPushObjectCustomPayloadIos =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("transactionalSharedPushObjectCustomPayloadIosMessage", "message")
      , ("transactionalSharedPushObjectCustomPayloadIosCIO", "CIO")
      , ("transactionalSharedPushObjectCustomPayloadIosAps", "aps")
      ]


-- | 
data UnsubscribeRequest = UnsubscribeRequest
  { unsubscribeRequestUnsubscribe :: Maybe Bool -- ^ If true, a person's `unsubscribed` attribute is set to true and the unsubscription is attributed to the delivery.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UnsubscribeRequest where
  parseJSON = genericParseJSON optionsUnsubscribeRequest
instance ToJSON UnsubscribeRequest where
  toJSON = genericToJSON optionsUnsubscribeRequest

optionsUnsubscribeRequest :: Options
optionsUnsubscribeRequest =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("unsubscribeRequestUnsubscribe", "unsubscribe")
      ]


-- | Unsuppress a person&#39;s identifier(s) in Customer.io, so that you can message a person or add their identifiers back to your workspace. This does not unsuppress addresses that were previously suppressed by your email provider.
data Unsuppress = Unsuppress
  { unsuppressType :: Text -- ^ The operation modifies a person in Customer.io
  , unsuppressIdentifiers :: IdentifyAllOfIdentifiers -- ^ 
  , unsuppressAction :: Text -- ^ Unsuppress a person's identifier(s) in Customer.io, so that you can message a person or add their identifiers back to your workspace. This does not unsuppress addresses that were previously suppressed by your email provider.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Unsuppress where
  parseJSON = genericParseJSON optionsUnsuppress
instance ToJSON Unsuppress where
  toJSON = genericToJSON optionsUnsuppress

optionsUnsuppress :: Options
optionsUnsuppress =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("unsuppressType", "type")
      , ("unsuppressIdentifiers", "identifiers")
      , ("unsuppressAction", "action")
      ]


-- | An array of JSON objects containing &#x60;id&#x60; or &#x60;email&#x60; keys and a &#x60;data&#x60; key. Each object represents a person you want to send the broadcast to and data you want to personalize their message with using liquid.
data UserMaps = UserMaps
  { userMapsPerUnderscoreuserUnderscoredata :: [UserMapsAllOfPerUserDataInner] -- ^ An array of people you want to send a broadcast to and custom data for each person. Each object in the array represents a person, with additional data you want to use to personalize their message. **When you trigger a broadcast, the people in your request must already exist in your workspace.** Requests to trigger a broadcast cannot create new people. 
  , userMapsData :: Maybe (Map.Map String Value) -- ^ Contains information you want to use to populate your broadcast.
  , userMapsEmailUnderscoreaddUnderscoreduplicates :: Maybe Bool -- ^ an email address associated with more than one profile id is an error.
  , userMapsEmailUnderscoreignoreUnderscoremissing :: Maybe Bool -- ^ If false a missing email address is an error.
  , userMapsIdUnderscoreignoreUnderscoremissing :: Maybe Bool -- ^ If false, a missing customer ID is an error.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserMaps where
  parseJSON = genericParseJSON optionsUserMaps
instance ToJSON UserMaps where
  toJSON = genericToJSON optionsUserMaps

optionsUserMaps :: Options
optionsUserMaps =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("userMapsPerUnderscoreuserUnderscoredata", "per_user_data")
      , ("userMapsData", "data")
      , ("userMapsEmailUnderscoreaddUnderscoreduplicates", "email_add_duplicates")
      , ("userMapsEmailUnderscoreignoreUnderscoremissing", "email_ignore_missing")
      , ("userMapsIdUnderscoreignoreUnderscoremissing", "id_ignore_missing")
      ]


-- | 
data UserMapsAllOfPerUserDataInner = UserMapsAllOfPerUserDataInner
  { userMapsAllOfPerUserDataInnerId :: Text -- ^ The ID of the recipient.
  , userMapsAllOfPerUserDataInnerData :: Maybe (Map.Map String Value) -- ^ Merge data associated with the recipient.
  , userMapsAllOfPerUserDataInnerEmail :: Text -- ^ The email address of the recipient. This address must be unique in your workspace. If more than one person has the same `email` attribute, your request will produce an error.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserMapsAllOfPerUserDataInner where
  parseJSON = genericParseJSON optionsUserMapsAllOfPerUserDataInner
instance ToJSON UserMapsAllOfPerUserDataInner where
  toJSON = genericToJSON optionsUserMapsAllOfPerUserDataInner

optionsUserMapsAllOfPerUserDataInner :: Options
optionsUserMapsAllOfPerUserDataInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("userMapsAllOfPerUserDataInnerId", "id")
      , ("userMapsAllOfPerUserDataInnerData", "data")
      , ("userMapsAllOfPerUserDataInnerEmail", "email")
      ]


-- | Describes relationships to an entity—a non-person object in Customer.io, like a company, educational course, job board, etc.
data V1CioRelationships = V1CioRelationships
  { v1CioRelationshipsAction :: Maybe Text -- ^ This determines whether the `relationships` array adds relationships to a person or removes them from a person.
  , v1CioRelationshipsRelationships :: Maybe [V1CioRelationshipsRelationshipsInner] -- ^ Each object in the array represents a relationship you want to add to, or remove from, a person.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON V1CioRelationships where
  parseJSON = genericParseJSON optionsV1CioRelationships
instance ToJSON V1CioRelationships where
  toJSON = genericToJSON optionsV1CioRelationships

optionsV1CioRelationships :: Options
optionsV1CioRelationships =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("v1CioRelationshipsAction", "action")
      , ("v1CioRelationshipsRelationships", "relationships")
      ]


-- | 
data V1CioRelationshipsRelationshipsInner = V1CioRelationshipsRelationshipsInner
  { v1CioRelationshipsRelationshipsInnerIdentifiers :: Maybe ObjectCommonAllOfIdentifiers -- ^ 
  , v1CioRelationshipsRelationshipsInnerRelationshipUnderscoreattributes :: Maybe (Map.Map String Value) -- ^ The attributes associated with a relationship. Passing null or an empty string removes the attribute from the relationship. 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON V1CioRelationshipsRelationshipsInner where
  parseJSON = genericParseJSON optionsV1CioRelationshipsRelationshipsInner
instance ToJSON V1CioRelationshipsRelationshipsInner where
  toJSON = genericToJSON optionsV1CioRelationshipsRelationshipsInner

optionsV1CioRelationshipsRelationshipsInner :: Options
optionsV1CioRelationshipsRelationshipsInner =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("v1CioRelationshipsRelationshipsInnerIdentifiers", "identifiers")
      , ("v1CioRelationshipsRelationshipsInnerRelationshipUnderscoreattributes", "relationship_attributes")
      ]


-- | 
data Webhook = Webhook
  { webhookDeliveryUnderscoreid :: Text -- ^ The CIO-Delivery-ID from the notification that you want to associate the `event` with.
  , webhookTimestamp :: Maybe Int -- ^ The unix timestamp when the event occurred.
  , webhookMetric :: Text -- ^ The type of device-side event you want to report back to Customer.io.
  , webhookReason :: Maybe Text -- ^ For metrics indicating a failure (like `bounced`), this field provides the reason for the failure.
  , webhookHref :: Maybe Text -- ^ For `clicked` metrics, this is the link the recipient clicked.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Webhook where
  parseJSON = genericParseJSON optionsWebhook
instance ToJSON Webhook where
  toJSON = genericToJSON optionsWebhook

optionsWebhook :: Options
optionsWebhook =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("webhookDeliveryUnderscoreid", "delivery_id")
      , ("webhookTimestamp", "timestamp")
      , ("webhookMetric", "metric")
      , ("webhookReason", "reason")
      , ("webhookHref", "href")
      ]


-- | 
data Webhook1 = Webhook1
  { webhook1Id :: Maybe Int -- ^ The identifier for an action.
  , webhook1BroadcastUnderscoreid :: Maybe Int -- ^ The identifier for a broadcast.
  , webhook1DeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , webhook1Name :: Maybe Text -- ^ The name of the action, if it exists.
  , webhook1Layout :: Maybe Text -- ^ The layout used for the action, if it exists.
  , webhook1Created :: Maybe Int -- ^ The date time when the referenced ID was created.
  , webhook1Updated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , webhook1Body :: Maybe Text -- ^ The body of the action. You cannot modify the body if you created it with our drag-and-drop editor.
  , webhook1Type :: Maybe Text -- ^ The type of action.
  , webhook1Url :: Maybe Text -- ^ The URL to send a webhook to, applies to `webhook` type actions.
  , webhook1Headers :: Maybe (Map.Map String Text) -- ^ An object containing headers, where the key is the header name and the value is the header value. Header names and values must be strings and cannot contain any non-ASCII characters or empty spaces. Some headers are reserved and cannot be overwritten.
  , webhook1Method :: Maybe Text -- ^ The HTTP method for your webhook.
  , webhook1SendingUnderscorestate :: Maybe Text -- ^ Determines the sending behavior for the action. `automatic` sends the action automatically when triggered; `draft` queues drafts when the action is triggered; or `off` to disable the action.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Webhook1 where
  parseJSON = genericParseJSON optionsWebhook1
instance ToJSON Webhook1 where
  toJSON = genericToJSON optionsWebhook1

optionsWebhook1 :: Options
optionsWebhook1 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("webhook1Id", "id")
      , ("webhook1BroadcastUnderscoreid", "broadcast_id")
      , ("webhook1DeduplicateUnderscoreid", "deduplicate_id")
      , ("webhook1Name", "name")
      , ("webhook1Layout", "layout")
      , ("webhook1Created", "created")
      , ("webhook1Updated", "updated")
      , ("webhook1Body", "body")
      , ("webhook1Type", "type")
      , ("webhook1Url", "url")
      , ("webhook1Headers", "headers")
      , ("webhook1Method", "method")
      , ("webhook1SendingUnderscorestate", "sending_state")
      ]


-- | 
data Webhook2 = Webhook2
  { webhook2Id :: Maybe Int -- ^ The identifier for an action.
  , webhook2CampaignUnderscoreid :: Maybe Int -- ^ The identifier for a campaign.
  , webhook2ParentUnderscoreactionUnderscoreid :: Maybe Int -- ^ The ID of the parent action, if the action occurred within a campaign and has a parent (like a randomized split, etc).
  , webhook2DeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , webhook2Name :: Maybe Text -- ^ The name of the action, if it exists.
  , webhook2Layout :: Maybe Text -- ^ The layout used for the action, if it exists.
  , webhook2Created :: Maybe Int -- ^ The date time when the referenced ID was created.
  , webhook2Updated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , webhook2Body :: Maybe Text -- ^ The payload for your webhook.
  , webhook2Type :: Maybe Text -- ^ The type of action.
  , webhook2Url :: Maybe Text -- ^ The URL to send a webhook to, applies to `webhook` type actions.
  , webhook2Headers :: Maybe (Map.Map String Text) -- ^ An object containing headers, where the key is the header name and the value is the header value. Header names and values must be strings and cannot contain any non-ASCII characters or empty spaces. Some headers are reserved and cannot be overwritten.
  , webhook2Method :: Maybe Text -- ^ The HTTP method for your webhook.
  , webhook2SendingUnderscorestate :: Maybe Text -- ^ Determines the sending behavior for the action. `automatic` sends the action automatically when triggered; `draft` queues drafts when the action is triggered; or `off` to disable the action.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Webhook2 where
  parseJSON = genericParseJSON optionsWebhook2
instance ToJSON Webhook2 where
  toJSON = genericToJSON optionsWebhook2

optionsWebhook2 :: Options
optionsWebhook2 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("webhook2Id", "id")
      , ("webhook2CampaignUnderscoreid", "campaign_id")
      , ("webhook2ParentUnderscoreactionUnderscoreid", "parent_action_id")
      , ("webhook2DeduplicateUnderscoreid", "deduplicate_id")
      , ("webhook2Name", "name")
      , ("webhook2Layout", "layout")
      , ("webhook2Created", "created")
      , ("webhook2Updated", "updated")
      , ("webhook2Body", "body")
      , ("webhook2Type", "type")
      , ("webhook2Url", "url")
      , ("webhook2Headers", "headers")
      , ("webhook2Method", "method")
      , ("webhook2SendingUnderscorestate", "sending_state")
      ]


-- | 
data Webhook3 = Webhook3
  { webhook3Id :: Maybe Int -- ^ The identifier for a campaign.
  , webhook3DeduplicateUnderscoreid :: Maybe Text -- ^ An identifier in the format `id:timestamp` where the id is the id of the object you are working with (Campaigns, Deliveries, Exports, Identities, Newsletters, Segments, and Templates), and the timestamp is the last time the object was updated.
  , webhook3Name :: Maybe Text -- ^ The name of the campaign.
  , webhook3Type :: Maybe Text -- ^ The type of campaign trigger. **Sunsetting on March 30, 2025**
  , webhook3Created :: Maybe Int -- ^ The date time when the referenced ID was created.
  , webhook3Updated :: Maybe Int -- ^ The date time when the referenced ID was last updated.
  , webhook3Active :: Maybe Bool -- ^ If true, the campaign is active and can still send messages.
  , webhook3State :: Maybe Text -- ^ The status of the campaign.
  , webhook3Actions :: Maybe [SegmentActionsInner] -- ^ An array of actions contained within the campaign.
  , webhook3FirstUnderscorestarted :: Maybe Int -- ^ The date and time when you first started the campaign and it first became eligible to be triggered.
  , webhook3Tags :: Maybe [Text] -- ^ An array of tags you set on this campaign.
  , webhook3WebhookUnderscoreid :: Maybe Int -- ^ The ID of the webhook trigger generated by Customer.io.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Webhook3 where
  parseJSON = genericParseJSON optionsWebhook3
instance ToJSON Webhook3 where
  toJSON = genericToJSON optionsWebhook3

optionsWebhook3 :: Options
optionsWebhook3 =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("webhook3Id", "id")
      , ("webhook3DeduplicateUnderscoreid", "deduplicate_id")
      , ("webhook3Name", "name")
      , ("webhook3Type", "type")
      , ("webhook3Created", "created")
      , ("webhook3Updated", "updated")
      , ("webhook3Active", "active")
      , ("webhook3State", "state")
      , ("webhook3Actions", "actions")
      , ("webhook3FirstUnderscorestarted", "first_started")
      , ("webhook3Tags", "tags")
      , ("webhook3WebhookUnderscoreid", "webhook_id")
      ]


-- | Describes the webhook events reported from Customer.io to a webhook.
data WebhookEvents = WebhookEvents
  { webhookEventsWebhookUnderscoreattempted :: Maybe Bool -- ^ Reports when a webhook could not be sent and will retry. Set to true to report this event type.
  , webhookEventsWebhookUnderscoreclicked :: Maybe Bool -- ^ Reports when a tracked link in a webhook payload is opened. Set to true to report this event type.
  , webhookEventsWebhookUnderscoredrafted :: Maybe Bool -- ^ A webhook draft is created. Set to true to report this event type.
  , webhookEventsWebhookUnderscorefailed :: Maybe Bool -- ^ Reports when a webhook couldn't be sent to the webhook URL. Set to true to report this event type.
  , webhookEventsWebhookUnderscoresent :: Maybe Bool -- ^ Reports when a webhook is sent from Customer.io to the webhook URL. Set to true to report this event type.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookEvents where
  parseJSON = genericParseJSON optionsWebhookEvents
instance ToJSON WebhookEvents where
  toJSON = genericToJSON optionsWebhookEvents

optionsWebhookEvents :: Options
optionsWebhookEvents =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("webhookEventsWebhookUnderscoreattempted", "webhook_attempted")
      , ("webhookEventsWebhookUnderscoreclicked", "webhook_clicked")
      , ("webhookEventsWebhookUnderscoredrafted", "webhook_drafted")
      , ("webhookEventsWebhookUnderscorefailed", "webhook_failed")
      , ("webhookEventsWebhookUnderscoresent", "webhook_sent")
      ]


-- | 
data WebhookMetrics = WebhookMetrics
  { webhookMetrics2xx :: Maybe Int -- ^ The number of 2xx responses.
  , webhookMetrics3xx :: Maybe Int -- ^ The number of 3xx responses.
  , webhookMetrics4xx :: Maybe Int -- ^ The number of 4xx responses.
  , webhookMetrics5xx :: Maybe Int -- ^ The number of 5xx responses.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookMetrics where
  parseJSON = genericParseJSON optionsWebhookMetrics
instance ToJSON WebhookMetrics where
  toJSON = genericToJSON optionsWebhookMetrics

optionsWebhookMetrics :: Options
optionsWebhookMetrics =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("webhookMetrics2xx", "2xx")
      , ("webhookMetrics3xx", "3xx")
      , ("webhookMetrics4xx", "4xx")
      , ("webhookMetrics5xx", "5xx")
      ]


-- | The crossAxisAlignment property supports the following options
data WidgetCrossAxisAlignment = WidgetCrossAxisAlignment
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WidgetCrossAxisAlignment where
  parseJSON = genericParseJSON optionsWidgetCrossAxisAlignment
instance ToJSON WidgetCrossAxisAlignment where
  toJSON = genericToJSON optionsWidgetCrossAxisAlignment

optionsWidgetCrossAxisAlignment :: Options
optionsWidgetCrossAxisAlignment =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | The mainAxisAlignment property supports the following options.
data WidgetMainAxisAlignment = WidgetMainAxisAlignment
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WidgetMainAxisAlignment where
  parseJSON = genericParseJSON optionsWidgetMainAxisAlignment
instance ToJSON WidgetMainAxisAlignment where
  toJSON = genericToJSON optionsWidgetMainAxisAlignment

optionsWidgetMainAxisAlignment :: Options
optionsWidgetMainAxisAlignment =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | Send a transactional message using a template that you set up in Customer.io.
data WithTemplate = WithTemplate
  { withTemplateTransactionalUnderscoremessageUnderscoreid :: SendpushAllOfTransactionalMessageId -- ^ 
  , withTemplateBody :: Maybe Text -- ^ The HTML body of your message. This overrides the body of the transactional template (referenced by `transactional_message_id`). If you send an AMP-enabled email (with `body_amp`), and the recipient's email client doesn't support AMP, this is the fallback email.
  , withTemplateBodyUnderscoreamp :: Maybe Text -- ^ If your message is an email, this is the AMP-enabled body of your message. If your recipient's email client doesn't support AMP, the `body` represents your fallback message.
  , withTemplateBodyUnderscoreplain :: Maybe Text -- ^ By default, we generate a plaintext version of your message body for each delivery. Use this key to override the default plain text body.
  , withTemplateSubject :: Maybe Text -- ^ The subject line for your message. This overrides the subject of the transactional template (referenced by `transactional_message_id`).
  , withTemplateFrom :: Maybe Text -- ^ The address that your email is from. This address must be verified by Customer.io. This overrides the from address set within the transactional template (referenced by `transactional_message_id`). You can include a display/friendly name in your from address, but we recommend that you use quotation marks around the friendly name to avoid potential issues with special characters, e.g. `\\\"Person\\\" <person@example.com>`.
  , withTemplateLanguage :: Maybe Text -- ^ Overrides language preferences for the person you want to send your transactional message to. Use one of our [supported two- or four-letter language codes](/localization-getting-started/#supported-languages).
  , withTemplateIdentifiers :: SendpushAllOfIdentifiers -- ^ 
  , withTemplateMessageUnderscoredata :: Maybe (Map.Map String Value) -- ^ An object containing the key-value pairs referenced using liquid in your message.
  , withTemplateSendUnderscoreat :: Maybe Int -- ^ A unix timestamp (seconds since epoch) determining when the message will be sent. The timestamp can be up to 90 days in the future. If this value is in the past, your message is sent immediately.
  , withTemplateDisableUnderscoremessageUnderscoreretention :: Maybe Bool -- ^ If true, the message body is not retained in delivery history. Setting this value overrides the value set in the settings of your `transactional_message_id`.
  , withTemplateSendUnderscoretoUnderscoreunsubscribed :: Maybe Bool -- ^ If false, your message is not sent to unsubscribed recipients. Setting this value overrides the value set in the settings of your `transactional_message_id`.
  , withTemplateQueueUnderscoredraft :: Maybe Bool -- ^ If true, your transactional message is held as a draft in Customer.io and not sent directly to your audience. You must go to the Deliveries and Drafts page to send your message.
  , withTemplateTo :: Text -- ^ The message recipient(s). Supports multiple addresses separated by commas. Your request can contain up to 15 total recipients between the `to` and `bcc` keys.  You can include a display or \"friendly\" name in \"to\" address, but we recommend that you use quotation marks around the friendly name to avoid potential issues with special characters, e.g. `\\\"Person\\\" <person@example.com>`.             
  , withTemplateBcc :: Maybe Text -- ^ Blind copy message recipients. Supports multiple addresses separated by commas. Your request can contain up to 15 total recipients between the `to` and `bcc` keys.
  , withTemplateFakeUnderscorebcc :: Maybe Bool -- ^ If true, rather than sending true copies to BCC addresses, Customer.io sends a copy of the message with the subject line containing the recipient address(es). 
  , withTemplateReplyUnderscoreto :: Maybe Text -- ^ The address that recipients can reply to, if different from the `from` address.
  , withTemplatePreheader :: Maybe Text -- ^ Also known as \"preview text\", this is the block block of text that users see next to, or underneath, the subject line in their inbox.
  , withTemplateAttachments :: Maybe TransactionalSharedEmailObjectAttachments -- ^ 
  , withTemplateHeaders :: Maybe (Map.Map String Text) -- ^ An object containing headers, where the key is the header name and the value is the header value. Header names and values must be strings and cannot contain any non-ASCII characters or empty spaces. Some headers are reserved and cannot be overwritten.
  , withTemplateDisableUnderscorecssUnderscorepreprocessing :: Maybe Bool -- ^ Set to `true` to disable CSS preprocessing. This setting overrides the CSS preprocessing setting on the `transactional_message_id` as set in the user interface. Transactional emails have CSS preprocessing enabled by default.
  , withTemplateTracked :: Maybe Bool -- ^ If true, Customer.io tracks opens and link clicks in your message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WithTemplate where
  parseJSON = genericParseJSON optionsWithTemplate
instance ToJSON WithTemplate where
  toJSON = genericToJSON optionsWithTemplate

optionsWithTemplate :: Options
optionsWithTemplate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("withTemplateTransactionalUnderscoremessageUnderscoreid", "transactional_message_id")
      , ("withTemplateBody", "body")
      , ("withTemplateBodyUnderscoreamp", "body_amp")
      , ("withTemplateBodyUnderscoreplain", "body_plain")
      , ("withTemplateSubject", "subject")
      , ("withTemplateFrom", "from")
      , ("withTemplateLanguage", "language")
      , ("withTemplateIdentifiers", "identifiers")
      , ("withTemplateMessageUnderscoredata", "message_data")
      , ("withTemplateSendUnderscoreat", "send_at")
      , ("withTemplateDisableUnderscoremessageUnderscoreretention", "disable_message_retention")
      , ("withTemplateSendUnderscoretoUnderscoreunsubscribed", "send_to_unsubscribed")
      , ("withTemplateQueueUnderscoredraft", "queue_draft")
      , ("withTemplateTo", "to")
      , ("withTemplateBcc", "bcc")
      , ("withTemplateFakeUnderscorebcc", "fake_bcc")
      , ("withTemplateReplyUnderscoreto", "reply_to")
      , ("withTemplatePreheader", "preheader")
      , ("withTemplateAttachments", "attachments")
      , ("withTemplateHeaders", "headers")
      , ("withTemplateDisableUnderscorecssUnderscorepreprocessing", "disable_css_preprocessing")
      , ("withTemplateTracked", "tracked")
      ]


-- | Create your own message from scratch.
data WithoutTemplate = WithoutTemplate
  { withoutTemplateBody :: Text -- ^ The body of your message.
  , withoutTemplateSubject :: Text -- ^ The subject line for your message.
  , withoutTemplateFrom :: Text -- ^ The address that your email is from. This address must be verified by Customer.io. You can include a display/friendly name in your from address in the format `Person <person@example.com>`.
  , withoutTemplateIdentifiers :: SendpushAllOfIdentifiers -- ^ 
  , withoutTemplateMessageUnderscoredata :: Maybe (Map.Map String Value) -- ^ An object containing the key-value pairs referenced using liquid in your message.
  , withoutTemplateSendUnderscoreat :: Maybe Int -- ^ A unix timestamp (seconds since epoch) determining when the message will be sent. The timestamp can be up to 90 days in the future. If this value is in the past, your message is sent immediately.
  , withoutTemplateDisableUnderscoremessageUnderscoreretention :: Maybe Bool -- ^ If true, the message body is not retained in delivery history. Setting this value overrides the value set in the settings of your `transactional_message_id`.
  , withoutTemplateSendUnderscoretoUnderscoreunsubscribed :: Maybe Bool -- ^ If false, your message is not sent to unsubscribed recipients. Setting this value overrides the value set in the settings of your `transactional_message_id`.
  , withoutTemplateQueueUnderscoredraft :: Maybe Bool -- ^ If true, your transactional message is held as a draft in Customer.io and not sent directly to your audience. You must go to the Deliveries and Drafts page to send your message.
  , withoutTemplateTo :: Text -- ^ The message recipient(s). Supports multiple addresses separated by commas. Your request can contain up to 15 total recipients between the `to` and `bcc` keys.  You can include a display or \"friendly\" name in \"to\" address, but we recommend that you use quotation marks around the friendly name to avoid potential issues with special characters, e.g. `\\\"Person\\\" <person@example.com>`.             
  , withoutTemplateBcc :: Maybe Text -- ^ Blind copy message recipients. Supports multiple addresses separated by commas. Your request can contain up to 15 total recipients between the `to` and `bcc` keys.
  , withoutTemplateFakeUnderscorebcc :: Maybe Bool -- ^ If true, rather than sending true copies to BCC addresses, Customer.io sends a copy of the message with the subject line containing the recipient address(es). 
  , withoutTemplateReplyUnderscoreto :: Maybe Text -- ^ The address that recipients can reply to, if different from the `from` address.
  , withoutTemplatePreheader :: Maybe Text -- ^ Also known as \"preview text\", this is the block block of text that users see next to, or underneath, the subject line in their inbox.
  , withoutTemplateBodyUnderscoreplain :: Maybe Text -- ^ By default, we generate a plaintext version of your message body for each delivery. Use this key to override the default plain text body.
  , withoutTemplateAttachments :: Maybe TransactionalSharedEmailObjectAttachments -- ^ 
  , withoutTemplateHeaders :: Maybe (Map.Map String Text) -- ^ An object containing headers, where the key is the header name and the value is the header value. Header names and values must be strings and cannot contain any non-ASCII characters or empty spaces. Some headers are reserved and cannot be overwritten.
  , withoutTemplateDisableUnderscorecssUnderscorepreprocessing :: Maybe Bool -- ^ Set to `true` to disable CSS preprocessing. This setting overrides the CSS preprocessing setting on the `transactional_message_id` as set in the user interface. Transactional emails have CSS preprocessing enabled by default.
  , withoutTemplateTracked :: Maybe Bool -- ^ If true, Customer.io tracks opens and link clicks in your message.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WithoutTemplate where
  parseJSON = genericParseJSON optionsWithoutTemplate
instance ToJSON WithoutTemplate where
  toJSON = genericToJSON optionsWithoutTemplate

optionsWithoutTemplate :: Options
optionsWithoutTemplate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("withoutTemplateBody", "body")
      , ("withoutTemplateSubject", "subject")
      , ("withoutTemplateFrom", "from")
      , ("withoutTemplateIdentifiers", "identifiers")
      , ("withoutTemplateMessageUnderscoredata", "message_data")
      , ("withoutTemplateSendUnderscoreat", "send_at")
      , ("withoutTemplateDisableUnderscoremessageUnderscoreretention", "disable_message_retention")
      , ("withoutTemplateSendUnderscoretoUnderscoreunsubscribed", "send_to_unsubscribed")
      , ("withoutTemplateQueueUnderscoredraft", "queue_draft")
      , ("withoutTemplateTo", "to")
      , ("withoutTemplateBcc", "bcc")
      , ("withoutTemplateFakeUnderscorebcc", "fake_bcc")
      , ("withoutTemplateReplyUnderscoreto", "reply_to")
      , ("withoutTemplatePreheader", "preheader")
      , ("withoutTemplateBodyUnderscoreplain", "body_plain")
      , ("withoutTemplateAttachments", "attachments")
      , ("withoutTemplateHeaders", "headers")
      , ("withoutTemplateDisableUnderscorecssUnderscorepreprocessing", "disable_css_preprocessing")
      , ("withoutTemplateTracked", "tracked")
      ]


-- | Contains workspace properties including the count of messages, people, and objects. Customer.io caches these counts, so your data may be up to two hours old.
data Workspace = Workspace
  { workspaceId :: Maybe Int -- ^ The id of the workspace.
  , workspaceName :: Maybe Text -- ^ The name of the workspace.
  , workspaceMessagesUnderscoresent :: Maybe Int -- ^ The count of [messages sent](/journeys/message-statuses/#sent) via any channel (email, SMS, in-app, push, slack) in the current billing period.
  , workspaceBillableUnderscoremessagesUnderscoresent :: Maybe Int -- ^ The count of [emails sent](/journeys/message-statuses/#sent) that are considered for billing in your current billing period. Ultimately, we only bill for the overages on your plan.
  , workspacePeople :: Maybe Int -- ^ The current count of people profiles in the workspace. Updates roughly every hour.
  , workspaceObjectUnderscoretypes :: Maybe Int -- ^ The current count of object types in the workspace. Updates roughly every hour.
  , workspaceObjects :: Maybe Int -- ^ The current count of object profiles in the workspace. Updates roughly every hour.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Workspace where
  parseJSON = genericParseJSON optionsWorkspace
instance ToJSON Workspace where
  toJSON = genericToJSON optionsWorkspace

optionsWorkspace :: Options
optionsWorkspace =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("workspaceId", "id")
      , ("workspaceName", "name")
      , ("workspaceMessagesUnderscoresent", "messages_sent")
      , ("workspaceBillableUnderscoremessagesUnderscoresent", "billable_messages_sent")
      , ("workspacePeople", "people")
      , ("workspaceObjectUnderscoretypes", "object_types")
      , ("workspaceObjects", "objects")
      ]

