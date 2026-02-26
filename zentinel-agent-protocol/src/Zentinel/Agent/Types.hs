{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Zentinel.Agent.Types
-- Description : Core types for Zentinel Agent Protocol v2
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : experimental
--
-- This module defines all data types used in the Zentinel Agent Protocol v2,
-- including events, decisions, capabilities, health status, and metrics.
-- All types use JSON serialization compatible with the Rust implementation.
module Zentinel.Agent.Types
  ( -- * Protocol Constants
    protocolVersion
  , maxMessageSize
  , maxUdsMessageSize
  , maxGrpcMessageSize

    -- * Event Types
  , EventType (..)

    -- * Decision Types
  , Decision (..)
  , WebSocketDecision (..)
  , HeaderOp (..)
  , BodyMutation (..)

    -- * Capabilities
  , AgentCapabilities (..)
  , AgentFeatures (..)
  , AgentLimits (..)
  , HealthConfig (..)
  , defaultCapabilities
  , defaultFeatures
  , defaultLimits
  , defaultHealthConfig

    -- * Handshake
  , HandshakeRequest (..)
  , HandshakeResponse (..)

    -- * Request Metadata
  , RequestMetadata (..)

    -- * Events
  , RequestHeadersEvent (..)
  , RequestBodyChunkEvent (..)
  , ResponseHeadersEvent (..)
  , ResponseBodyChunkEvent (..)
  , RequestCompleteEvent (..)
  , WebSocketFrameEvent (..)
  , WebSocketFrameType (..)
  , GuardrailInspectEvent (..)
  , GuardrailInspectionType (..)

    -- * Responses
  , AgentResponse (..)
  , GuardrailResponse (..)
  , GuardrailDetection (..)
  , DetectionSeverity (..)
  , TextSpan (..)

    -- * Audit
  , AuditMetadata (..)

    -- * Health
  , HealthStatus (..)
  , HealthState (..)
  , LoadMetrics (..)
  , ResourceMetrics (..)

    -- * Metrics
  , MetricsReport (..)
  , CounterMetric (..)
  , GaugeMetric (..)
  , HistogramMetric (..)
  , HistogramBucket (..)

    -- ** Standard Metric Names
  , metricRequestsTotal
  , metricRequestsBlockedTotal
  , metricRequestsDurationSeconds
  , metricErrorsTotal
  , metricInFlightRequests
  , metricQueueDepth

    -- * Control Plane
  , CancelRequest (..)
  , CancelReason (..)
  , ConfigUpdateRequest (..)
  , ConfigUpdateType (..)
  , RuleDefinition (..)
  , ConfigUpdateResponse (..)
  , ShutdownReason (..)
  , DrainReason (..)
  , LogLevel (..)

    -- * Flow Control
  , FlowControlSignal (..)
  , FlowAction (..)
  , StreamState (..)

    -- * Envelope Types
  , AgentMessage (..)
  , ProxyMessage (..)

    -- * Helper Functions
  , allow
  , block
  , redirect
  , challenge
  , healthy
  , degraded
  , draining
  , unhealthy
  , counter
  , gauge
  , histogram
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Scientific (toBoundedInteger, toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Protocol Constants
--------------------------------------------------------------------------------

-- | Protocol version (v2)
protocolVersion :: Word32
protocolVersion = 2

-- | Maximum message size for JSON protocol (10MB)
maxMessageSize :: Int
maxMessageSize = 10 * 1024 * 1024

-- | Maximum message size for v2 binary UDS (16MB)
maxUdsMessageSize :: Int
maxUdsMessageSize = 16 * 1024 * 1024

-- | Maximum message size for v2 gRPC (10MB)
maxGrpcMessageSize :: Int
maxGrpcMessageSize = 10 * 1024 * 1024

--------------------------------------------------------------------------------
-- Event Types
--------------------------------------------------------------------------------

-- | Types of events that agents can handle
data EventType
  = Configure
  | RequestHeaders
  | RequestBodyChunk
  | ResponseHeaders
  | ResponseBodyChunk
  | RequestComplete
  | WebSocketFrame
  | GuardrailInspect
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (NFData)

instance ToJSON EventType where
  toJSON = \case
    Configure -> "configure"
    RequestHeaders -> "request_headers"
    RequestBodyChunk -> "request_body_chunk"
    ResponseHeaders -> "response_headers"
    ResponseBodyChunk -> "response_body_chunk"
    RequestComplete -> "request_complete"
    WebSocketFrame -> "websocket_frame"
    GuardrailInspect -> "guardrail_inspect"

instance FromJSON EventType where
  parseJSON = withText "EventType" $ \case
    "configure" -> pure Configure
    "request_headers" -> pure RequestHeaders
    "request_body_chunk" -> pure RequestBodyChunk
    "response_headers" -> pure ResponseHeaders
    "response_body_chunk" -> pure ResponseBodyChunk
    "request_complete" -> pure RequestComplete
    "websocket_frame" -> pure WebSocketFrame
    "guardrail_inspect" -> pure GuardrailInspect
    other -> fail $ "Unknown EventType: " <> T.unpack other

--------------------------------------------------------------------------------
-- Decision Types
--------------------------------------------------------------------------------

-- | Decision returned by an agent after evaluating a request/response
data Decision
  = -- | Allow the request/response to continue
    Allow
  | -- | Block with HTTP status code
    Block
      { blockStatus :: !Word16
      , blockBody :: !(Maybe Text)
      , blockHeaders :: !(Maybe (HashMap Text Text))
      }
  | -- | Redirect to another URL
    Redirect
      { redirectUrl :: !Text
      , redirectStatus :: !Word16
      }
  | -- | Challenge the client (e.g., CAPTCHA)
    Challenge
      { challengeType :: !Text
      , challengeParams :: !(HashMap Text Text)
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON Decision where
  toJSON Allow = object ["type" .= ("allow" :: Text)]
  toJSON Block {..} =
    object $
      ["type" .= ("block" :: Text), "status" .= blockStatus]
        <> maybe [] (\b -> ["body" .= b]) blockBody
        <> maybe [] (\h -> ["headers" .= h]) blockHeaders
  toJSON Redirect {..} =
    object
      [ "type" .= ("redirect" :: Text)
      , "url" .= redirectUrl
      , "status" .= redirectStatus
      ]
  toJSON Challenge {..} =
    object
      [ "type" .= ("challenge" :: Text)
      , "challenge_type" .= challengeType
      , "params" .= challengeParams
      ]

instance FromJSON Decision where
  parseJSON = withObject "Decision" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "allow" -> pure Allow
      "block" -> do
        blockStatus <- o .: "status"
        blockBody <- o .:? "body"
        blockHeaders <- o .:? "headers"
        pure Block {..}
      "redirect" -> do
        redirectUrl <- o .: "url"
        redirectStatus <- o .: "status"
        pure Redirect {..}
      "challenge" -> do
        challengeType <- o .: "challenge_type"
        challengeParams <- o .:? "params" .!= HM.empty
        pure Challenge {..}
      other -> fail $ "Unknown Decision type: " <> T.unpack other

-- | Decision for WebSocket frames
data WebSocketDecision
  = -- | Allow frame to pass through
    WsAllow
  | -- | Drop frame silently
    WsDrop
  | -- | Close connection (RFC 6455)
    WsClose
      { wsCloseCode :: !Word16
      , wsCloseReason :: !Text
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON WebSocketDecision where
  toJSON WsAllow = object ["type" .= ("allow" :: Text)]
  toJSON WsDrop = object ["type" .= ("drop" :: Text)]
  toJSON WsClose {..} =
    object
      [ "type" .= ("close" :: Text)
      , "code" .= wsCloseCode
      , "reason" .= wsCloseReason
      ]

instance FromJSON WebSocketDecision where
  parseJSON = withObject "WebSocketDecision" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "allow" -> pure WsAllow
      "drop" -> pure WsDrop
      "close" -> do
        wsCloseCode <- o .: "code"
        wsCloseReason <- o .: "reason"
        pure WsClose {..}
      other -> fail $ "Unknown WebSocketDecision type: " <> T.unpack other

-- | Header operation for modifying request/response headers
data HeaderOp
  = -- | Set header (replace if exists)
    HeaderSet
      { headerName :: !Text
      , headerValue :: !Text
      }
  | -- | Add header (append if exists)
    HeaderAdd
      { headerName :: !Text
      , headerValue :: !Text
      }
  | -- | Remove header
    HeaderRemove
      { headerName :: !Text
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON HeaderOp where
  toJSON HeaderSet {..} =
    object ["set" .= object ["name" .= headerName, "value" .= headerValue]]
  toJSON HeaderAdd {..} =
    object ["add" .= object ["name" .= headerName, "value" .= headerValue]]
  toJSON HeaderRemove {..} =
    object ["remove" .= object ["name" .= headerName]]

instance FromJSON HeaderOp where
  parseJSON = withObject "HeaderOp" $ \o -> do
    mSet <- o .:? "set"
    mAdd <- o .:? "add"
    mRemove <- o .:? "remove"
    case (mSet, mAdd, mRemove) of
      (Just setObj, _, _) -> do
        headerName <- setObj .: "name"
        headerValue <- setObj .: "value"
        pure HeaderSet {..}
      (_, Just addObj, _) -> do
        headerName <- addObj .: "name"
        headerValue <- addObj .: "value"
        pure HeaderAdd {..}
      (_, _, Just removeObj) -> do
        headerName <- removeObj .: "name"
        pure HeaderRemove {..}
      _ -> fail "HeaderOp must have 'set', 'add', or 'remove' field"

-- | Body mutation for request/response body chunks
data BodyMutation = BodyMutation
  { -- | Nothing = pass-through, Just "" = drop chunk, Just data = replace
    mutationData :: !(Maybe Text)
  , mutationChunkIndex :: !Word32
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON BodyMutation where
  toJSON BodyMutation {..} =
    object $
      ["chunk_index" .= mutationChunkIndex]
        <> maybe [] (\d -> ["data" .= d]) mutationData

instance FromJSON BodyMutation where
  parseJSON = withObject "BodyMutation" $ \o -> do
    mutationData <- o .:? "data"
    mutationChunkIndex <- o .: "chunk_index"
    pure BodyMutation {..}

--------------------------------------------------------------------------------
-- Capabilities
--------------------------------------------------------------------------------

-- | Agent capabilities reported during handshake
data AgentCapabilities = AgentCapabilities
  { capProtocolVersion :: !Word32
  , capAgentId :: !Text
  , capName :: !Text
  , capVersion :: !Text
  , capSupportedEvents :: ![EventType]
  , capFeatures :: !AgentFeatures
  , capLimits :: !AgentLimits
  , capHealthConfig :: !HealthConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON AgentCapabilities where
  toJSON AgentCapabilities {..} =
    object
      [ "protocol_version" .= capProtocolVersion
      , "agent_id" .= capAgentId
      , "name" .= capName
      , "version" .= capVersion
      , "supported_events" .= capSupportedEvents
      , "features" .= capFeatures
      , "limits" .= capLimits
      , "health" .= capHealthConfig
      ]

instance FromJSON AgentCapabilities where
  parseJSON = withObject "AgentCapabilities" $ \o -> do
    capProtocolVersion <- o .: "protocol_version"
    capAgentId <- o .: "agent_id"
    capName <- o .: "name"
    capVersion <- o .: "version"
    capSupportedEvents <- o .: "supported_events"
    capFeatures <- o .:? "features" .!= defaultFeatures
    capLimits <- o .:? "limits" .!= defaultLimits
    capHealthConfig <- o .:? "health" .!= defaultHealthConfig
    pure AgentCapabilities {..}

-- | Feature flags for agent capabilities
data AgentFeatures = AgentFeatures
  { featStreamingBody :: !Bool
  , featWebsocket :: !Bool
  , featGuardrails :: !Bool
  , featConfigPush :: !Bool
  , featMetricsExport :: !Bool
  , featConcurrentRequests :: !Word32
  , featCancellation :: !Bool
  , featFlowControl :: !Bool
  , featHealthReporting :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON AgentFeatures where
  toJSON AgentFeatures {..} =
    object
      [ "streaming_body" .= featStreamingBody
      , "websocket" .= featWebsocket
      , "guardrails" .= featGuardrails
      , "config_push" .= featConfigPush
      , "metrics_export" .= featMetricsExport
      , "concurrent_requests" .= featConcurrentRequests
      , "cancellation" .= featCancellation
      , "flow_control" .= featFlowControl
      , "health_reporting" .= featHealthReporting
      ]

instance FromJSON AgentFeatures where
  parseJSON = withObject "AgentFeatures" $ \o -> do
    featStreamingBody <- o .:? "streaming_body" .!= False
    featWebsocket <- o .:? "websocket" .!= False
    featGuardrails <- o .:? "guardrails" .!= False
    featConfigPush <- o .:? "config_push" .!= False
    featMetricsExport <- o .:? "metrics_export" .!= False
    featConcurrentRequests <- o .:? "concurrent_requests" .!= 1
    featCancellation <- o .:? "cancellation" .!= False
    featFlowControl <- o .:? "flow_control" .!= False
    featHealthReporting <- o .:? "health_reporting" .!= False
    pure AgentFeatures {..}

-- | Resource limits for the agent
data AgentLimits = AgentLimits
  { limMaxBodySize :: !Int
  , limMaxConcurrency :: !Word32
  , limPreferredChunkSize :: !Int
  , limMaxMemory :: !(Maybe Int)
  , limMaxProcessingTimeMs :: !(Maybe Word64)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON AgentLimits where
  toJSON AgentLimits {..} =
    object $
      [ "max_body_size" .= limMaxBodySize
      , "max_concurrency" .= limMaxConcurrency
      , "preferred_chunk_size" .= limPreferredChunkSize
      ]
        <> maybe [] (\m -> ["max_memory" .= m]) limMaxMemory
        <> maybe [] (\t -> ["max_processing_time_ms" .= t]) limMaxProcessingTimeMs

instance FromJSON AgentLimits where
  parseJSON = withObject "AgentLimits" $ \o -> do
    limMaxBodySize <- o .:? "max_body_size" .!= (10 * 1024 * 1024)
    limMaxConcurrency <- o .:? "max_concurrency" .!= 100
    limPreferredChunkSize <- o .:? "preferred_chunk_size" .!= (64 * 1024)
    limMaxMemory <- o .:? "max_memory"
    limMaxProcessingTimeMs <- o .:? "max_processing_time_ms"
    pure AgentLimits {..}

-- | Health reporting configuration
data HealthConfig = HealthConfig
  { healthReportIntervalMs :: !Word32
  , healthIncludeLoad :: !Bool
  , healthIncludeResources :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON HealthConfig where
  toJSON HealthConfig {..} =
    object
      [ "report_interval_ms" .= healthReportIntervalMs
      , "include_load_metrics" .= healthIncludeLoad
      , "include_resource_metrics" .= healthIncludeResources
      ]

instance FromJSON HealthConfig where
  parseJSON = withObject "HealthConfig" $ \o -> do
    healthReportIntervalMs <- o .:? "report_interval_ms" .!= 10000
    healthIncludeLoad <- o .:? "include_load_metrics" .!= True
    healthIncludeResources <- o .:? "include_resource_metrics" .!= False
    pure HealthConfig {..}

-- | Default capabilities for a simple agent
defaultCapabilities :: Text -> AgentCapabilities
defaultCapabilities name =
  AgentCapabilities
    { capProtocolVersion = protocolVersion
    , capAgentId = name <> "-001"
    , capName = name
    , capVersion = "0.1.0"
    , capSupportedEvents = [RequestHeaders]
    , capFeatures = defaultFeatures
    , capLimits = defaultLimits
    , capHealthConfig = defaultHealthConfig
    }

-- | Default feature flags
defaultFeatures :: AgentFeatures
defaultFeatures =
  AgentFeatures
    { featStreamingBody = False
    , featWebsocket = False
    , featGuardrails = False
    , featConfigPush = False
    , featMetricsExport = False
    , featConcurrentRequests = 1
    , featCancellation = False
    , featFlowControl = False
    , featHealthReporting = False
    }

-- | Default resource limits
defaultLimits :: AgentLimits
defaultLimits =
  AgentLimits
    { limMaxBodySize = 10 * 1024 * 1024  -- 10MB
    , limMaxConcurrency = 100
    , limPreferredChunkSize = 64 * 1024  -- 64KB
    , limMaxMemory = Nothing
    , limMaxProcessingTimeMs = Just 5000  -- 5 seconds
    }

-- | Default health configuration
defaultHealthConfig :: HealthConfig
defaultHealthConfig =
  HealthConfig
    { healthReportIntervalMs = 10000  -- 10 seconds
    , healthIncludeLoad = True
    , healthIncludeResources = False
    }

--------------------------------------------------------------------------------
-- Handshake
--------------------------------------------------------------------------------

-- | Handshake request from proxy to agent
data HandshakeRequest = HandshakeRequest
  { hsReqSupportedVersions :: ![Word32]
  , hsReqProxyId :: !Text
  , hsReqProxyVersion :: !Text
  , hsReqConfig :: !Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON HandshakeRequest where
  toJSON HandshakeRequest {..} =
    object
      [ "supported_versions" .= hsReqSupportedVersions
      , "proxy_id" .= hsReqProxyId
      , "proxy_version" .= hsReqProxyVersion
      , "config" .= hsReqConfig
      ]

instance FromJSON HandshakeRequest where
  parseJSON = withObject "HandshakeRequest" $ \o -> do
    hsReqSupportedVersions <- o .: "supported_versions"
    hsReqProxyId <- o .: "proxy_id"
    hsReqProxyVersion <- o .: "proxy_version"
    hsReqConfig <- o .:? "config" .!= Null
    pure HandshakeRequest {..}

-- | Handshake response from agent to proxy
data HandshakeResponse = HandshakeResponse
  { hsRespProtocolVersion :: !Word32
  , hsRespCapabilities :: !AgentCapabilities
  , hsRespSuccess :: !Bool
  , hsRespError :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON HandshakeResponse where
  toJSON HandshakeResponse {..} =
    object $
      [ "protocol_version" .= hsRespProtocolVersion
      , "capabilities" .= hsRespCapabilities
      , "success" .= hsRespSuccess
      ]
        <> maybe [] (\e -> ["error" .= e]) hsRespError

instance FromJSON HandshakeResponse where
  parseJSON = withObject "HandshakeResponse" $ \o -> do
    hsRespProtocolVersion <- o .: "protocol_version"
    hsRespCapabilities <- o .: "capabilities"
    hsRespSuccess <- o .: "success"
    hsRespError <- o .:? "error"
    pure HandshakeResponse {..}

--------------------------------------------------------------------------------
-- Request Metadata
--------------------------------------------------------------------------------

-- | Metadata about the incoming request
data RequestMetadata = RequestMetadata
  { metaCorrelationId :: !Text
  , metaRequestId :: !Text
  , metaClientIp :: !Text
  , metaClientPort :: !Word16
  , metaServerName :: !(Maybe Text)
  , metaProtocol :: !Text
  , metaTlsVersion :: !(Maybe Text)
  , metaTlsCipher :: !(Maybe Text)
  , metaRouteId :: !(Maybe Text)
  , metaUpstreamId :: !(Maybe Text)
  , metaTimestamp :: !Text
  , metaTraceparent :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON RequestMetadata where
  toJSON RequestMetadata {..} =
    object $
      [ "correlation_id" .= metaCorrelationId
      , "request_id" .= metaRequestId
      , "client_ip" .= metaClientIp
      , "client_port" .= metaClientPort
      , "protocol" .= metaProtocol
      , "timestamp" .= metaTimestamp
      ]
        <> maybe [] (\v -> ["server_name" .= v]) metaServerName
        <> maybe [] (\v -> ["tls_version" .= v]) metaTlsVersion
        <> maybe [] (\v -> ["tls_cipher" .= v]) metaTlsCipher
        <> maybe [] (\v -> ["route_id" .= v]) metaRouteId
        <> maybe [] (\v -> ["upstream_id" .= v]) metaUpstreamId
        <> maybe [] (\v -> ["traceparent" .= v]) metaTraceparent

instance FromJSON RequestMetadata where
  parseJSON = withObject "RequestMetadata" $ \o -> do
    metaCorrelationId <- o .: "correlation_id"
    metaRequestId <- o .: "request_id"
    metaClientIp <- o .: "client_ip"
    metaClientPort <- o .: "client_port"
    metaServerName <- o .:? "server_name"
    metaProtocol <- o .: "protocol"
    metaTlsVersion <- o .:? "tls_version"
    metaTlsCipher <- o .:? "tls_cipher"
    metaRouteId <- o .:? "route_id"
    metaUpstreamId <- o .:? "upstream_id"
    metaTimestamp <- o .: "timestamp"
    metaTraceparent <- o .:? "traceparent"
    pure RequestMetadata {..}

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

-- | Request headers event
data RequestHeadersEvent = RequestHeadersEvent
  { reqHdrMetadata :: !RequestMetadata
  , reqHdrMethod :: !Text
  , reqHdrUri :: !Text
  , reqHdrHeaders :: !(HashMap Text (Vector Text))
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON RequestHeadersEvent where
  toJSON RequestHeadersEvent {..} =
    object
      [ "metadata" .= reqHdrMetadata
      , "method" .= reqHdrMethod
      , "uri" .= reqHdrUri
      , "headers" .= reqHdrHeaders
      ]

instance FromJSON RequestHeadersEvent where
  parseJSON = withObject "RequestHeadersEvent" $ \o -> do
    reqHdrMetadata <- o .: "metadata"
    reqHdrMethod <- o .: "method"
    reqHdrUri <- o .: "uri"
    reqHdrHeaders <- o .: "headers"
    pure RequestHeadersEvent {..}

-- | Request body chunk event
data RequestBodyChunkEvent = RequestBodyChunkEvent
  { reqBodyCorrelationId :: !Text
  , reqBodyData :: !Text  -- Base64-encoded
  , reqBodyIsLast :: !Bool
  , reqBodyTotalSize :: !(Maybe Int)
  , reqBodyChunkIndex :: !Word32
  , reqBodyBytesReceived :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON RequestBodyChunkEvent where
  toJSON RequestBodyChunkEvent {..} =
    object $
      [ "correlation_id" .= reqBodyCorrelationId
      , "data" .= reqBodyData
      , "is_last" .= reqBodyIsLast
      , "chunk_index" .= reqBodyChunkIndex
      , "bytes_received" .= reqBodyBytesReceived
      ]
        <> maybe [] (\s -> ["total_size" .= s]) reqBodyTotalSize

instance FromJSON RequestBodyChunkEvent where
  parseJSON = withObject "RequestBodyChunkEvent" $ \o -> do
    reqBodyCorrelationId <- o .: "correlation_id"
    reqBodyData <- o .: "data"
    reqBodyIsLast <- o .: "is_last"
    reqBodyTotalSize <- o .:? "total_size"
    reqBodyChunkIndex <- o .: "chunk_index"
    reqBodyBytesReceived <- o .: "bytes_received"
    pure RequestBodyChunkEvent {..}

-- | Response headers event
data ResponseHeadersEvent = ResponseHeadersEvent
  { respHdrCorrelationId :: !Text
  , respHdrStatus :: !Word16
  , respHdrHeaders :: !(HashMap Text (Vector Text))
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ResponseHeadersEvent where
  toJSON ResponseHeadersEvent {..} =
    object
      [ "correlation_id" .= respHdrCorrelationId
      , "status" .= respHdrStatus
      , "headers" .= respHdrHeaders
      ]

instance FromJSON ResponseHeadersEvent where
  parseJSON = withObject "ResponseHeadersEvent" $ \o -> do
    respHdrCorrelationId <- o .: "correlation_id"
    respHdrStatus <- o .: "status"
    respHdrHeaders <- o .: "headers"
    pure ResponseHeadersEvent {..}

-- | Response body chunk event
data ResponseBodyChunkEvent = ResponseBodyChunkEvent
  { respBodyCorrelationId :: !Text
  , respBodyData :: !Text  -- Base64-encoded
  , respBodyIsLast :: !Bool
  , respBodyTotalSize :: !(Maybe Int)
  , respBodyChunkIndex :: !Word32
  , respBodyBytesSent :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ResponseBodyChunkEvent where
  toJSON ResponseBodyChunkEvent {..} =
    object $
      [ "correlation_id" .= respBodyCorrelationId
      , "data" .= respBodyData
      , "is_last" .= respBodyIsLast
      , "chunk_index" .= respBodyChunkIndex
      , "bytes_sent" .= respBodyBytesSent
      ]
        <> maybe [] (\s -> ["total_size" .= s]) respBodyTotalSize

instance FromJSON ResponseBodyChunkEvent where
  parseJSON = withObject "ResponseBodyChunkEvent" $ \o -> do
    respBodyCorrelationId <- o .: "correlation_id"
    respBodyData <- o .: "data"
    respBodyIsLast <- o .: "is_last"
    respBodyTotalSize <- o .:? "total_size"
    respBodyChunkIndex <- o .: "chunk_index"
    respBodyBytesSent <- o .: "bytes_sent"
    pure ResponseBodyChunkEvent {..}

-- | Request complete event (for logging/metrics)
data RequestCompleteEvent = RequestCompleteEvent
  { reqCompCorrelationId :: !Text
  , reqCompRequestId :: !Text
  , reqCompStatusCode :: !Word16
  , reqCompRequestBytes :: !Int
  , reqCompResponseBytes :: !Int
  , reqCompDurationMs :: !Word64
  , reqCompUpstreamDurationMs :: !(Maybe Word64)
  , reqCompCached :: !Bool
  , reqCompError :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON RequestCompleteEvent where
  toJSON RequestCompleteEvent {..} =
    object $
      [ "correlation_id" .= reqCompCorrelationId
      , "request_id" .= reqCompRequestId
      , "status_code" .= reqCompStatusCode
      , "request_bytes" .= reqCompRequestBytes
      , "response_bytes" .= reqCompResponseBytes
      , "duration_ms" .= reqCompDurationMs
      , "cached" .= reqCompCached
      ]
        <> maybe [] (\d -> ["upstream_duration_ms" .= d]) reqCompUpstreamDurationMs
        <> maybe [] (\e -> ["error" .= e]) reqCompError

instance FromJSON RequestCompleteEvent where
  parseJSON = withObject "RequestCompleteEvent" $ \o -> do
    reqCompCorrelationId <- o .: "correlation_id"
    reqCompRequestId <- o .: "request_id"
    reqCompStatusCode <- o .: "status_code"
    reqCompRequestBytes <- o .: "request_bytes"
    reqCompResponseBytes <- o .: "response_bytes"
    reqCompDurationMs <- o .: "duration_ms"
    reqCompUpstreamDurationMs <- o .:? "upstream_duration_ms"
    reqCompCached <- o .:? "cached" .!= False
    reqCompError <- o .:? "error"
    pure RequestCompleteEvent {..}

-- | WebSocket frame type
data WebSocketFrameType
  = WsText
  | WsBinary
  | WsPing
  | WsPong
  | WsCloseFrame
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (NFData)

instance ToJSON WebSocketFrameType where
  toJSON = \case
    WsText -> "text"
    WsBinary -> "binary"
    WsPing -> "ping"
    WsPong -> "pong"
    WsCloseFrame -> "close"

instance FromJSON WebSocketFrameType where
  parseJSON = withText "WebSocketFrameType" $ \case
    "text" -> pure WsText
    "binary" -> pure WsBinary
    "ping" -> pure WsPing
    "pong" -> pure WsPong
    "close" -> pure WsCloseFrame
    other -> fail $ "Unknown WebSocketFrameType: " <> T.unpack other

-- | WebSocket frame event
data WebSocketFrameEvent = WebSocketFrameEvent
  { wsFrameCorrelationId :: !Text
  , wsFrameType :: !WebSocketFrameType
  , wsFrameData :: !Text  -- Base64-encoded
  , wsFrameFromClient :: !Bool
  , wsFrameTimestamp :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON WebSocketFrameEvent where
  toJSON WebSocketFrameEvent {..} =
    object
      [ "correlation_id" .= wsFrameCorrelationId
      , "frame_type" .= wsFrameType
      , "data" .= wsFrameData
      , "from_client" .= wsFrameFromClient
      , "timestamp" .= wsFrameTimestamp
      ]

instance FromJSON WebSocketFrameEvent where
  parseJSON = withObject "WebSocketFrameEvent" $ \o -> do
    wsFrameCorrelationId <- o .: "correlation_id"
    wsFrameType <- o .: "frame_type"
    wsFrameData <- o .: "data"
    wsFrameFromClient <- o .: "from_client"
    wsFrameTimestamp <- o .: "timestamp"
    pure WebSocketFrameEvent {..}

-- | Guardrail inspection type
data GuardrailInspectionType
  = PromptInjection
  | PiiDetection
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (NFData)

instance ToJSON GuardrailInspectionType where
  toJSON = \case
    PromptInjection -> "prompt_injection"
    PiiDetection -> "pii_detection"

instance FromJSON GuardrailInspectionType where
  parseJSON = withText "GuardrailInspectionType" $ \case
    "prompt_injection" -> pure PromptInjection
    "pii_detection" -> pure PiiDetection
    other -> fail $ "Unknown GuardrailInspectionType: " <> T.unpack other

-- | Guardrail inspection event
data GuardrailInspectEvent = GuardrailInspectEvent
  { grInspectCorrelationId :: !Text
  , grInspectType :: !GuardrailInspectionType
  , grInspectContent :: !Text
  , grInspectModel :: !(Maybe Text)
  , grInspectCategories :: ![Text]
  , grInspectRouteId :: !(Maybe Text)
  , grInspectMetadata :: !(HashMap Text Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON GuardrailInspectEvent where
  toJSON GuardrailInspectEvent {..} =
    object $
      [ "correlation_id" .= grInspectCorrelationId
      , "inspection_type" .= grInspectType
      , "content" .= grInspectContent
      , "categories" .= grInspectCategories
      , "metadata" .= grInspectMetadata
      ]
        <> maybe [] (\m -> ["model" .= m]) grInspectModel
        <> maybe [] (\r -> ["route_id" .= r]) grInspectRouteId

instance FromJSON GuardrailInspectEvent where
  parseJSON = withObject "GuardrailInspectEvent" $ \o -> do
    grInspectCorrelationId <- o .: "correlation_id"
    grInspectType <- o .: "inspection_type"
    grInspectContent <- o .: "content"
    grInspectModel <- o .:? "model"
    grInspectCategories <- o .:? "categories" .!= []
    grInspectRouteId <- o .:? "route_id"
    grInspectMetadata <- o .:? "metadata" .!= HM.empty
    pure GuardrailInspectEvent {..}

--------------------------------------------------------------------------------
-- Responses
--------------------------------------------------------------------------------

-- | Audit metadata for logging and observability
data AuditMetadata = AuditMetadata
  { auditTags :: ![Text]
  , auditRuleIds :: ![Text]
  , auditConfidence :: !(Maybe Double)
  , auditReasonCodes :: ![Text]
  , auditCustom :: !(HashMap Text Value)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON AuditMetadata where
  toJSON AuditMetadata {..} =
    object $
      [ "tags" .= auditTags
      , "rule_ids" .= auditRuleIds
      , "reason_codes" .= auditReasonCodes
      , "custom" .= auditCustom
      ]
        <> maybe [] (\c -> ["confidence" .= c]) auditConfidence

instance FromJSON AuditMetadata where
  parseJSON = withObject "AuditMetadata" $ \o -> do
    auditTags <- o .:? "tags" .!= []
    auditRuleIds <- o .:? "rule_ids" .!= []
    auditConfidence <- o .:? "confidence"
    auditReasonCodes <- o .:? "reason_codes" .!= []
    auditCustom <- o .:? "custom" .!= HM.empty
    pure AuditMetadata {..}

-- | Agent response to an event
data AgentResponse = AgentResponse
  { respVersion :: !Word32
  , respDecision :: !Decision
  , respRequestHeaders :: ![HeaderOp]
  , respResponseHeaders :: ![HeaderOp]
  , respRoutingMetadata :: !(HashMap Text Text)
  , respAudit :: !AuditMetadata
  , respNeedsMore :: !Bool
  , respRequestBodyMutation :: !(Maybe BodyMutation)
  , respResponseBodyMutation :: !(Maybe BodyMutation)
  , respWebSocketDecision :: !(Maybe WebSocketDecision)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON AgentResponse where
  toJSON AgentResponse {..} =
    object $
      [ "version" .= respVersion
      , "decision" .= respDecision
      , "request_headers" .= respRequestHeaders
      , "response_headers" .= respResponseHeaders
      , "routing_metadata" .= respRoutingMetadata
      , "audit" .= respAudit
      , "needs_more" .= respNeedsMore
      ]
        <> maybe [] (\m -> ["request_body_mutation" .= m]) respRequestBodyMutation
        <> maybe [] (\m -> ["response_body_mutation" .= m]) respResponseBodyMutation
        <> maybe [] (\d -> ["websocket_decision" .= d]) respWebSocketDecision

instance FromJSON AgentResponse where
  parseJSON = withObject "AgentResponse" $ \o -> do
    respVersion <- o .:? "version" .!= 1
    respDecision <- o .:? "decision" .!= Allow
    respRequestHeaders <- o .:? "request_headers" .!= []
    respResponseHeaders <- o .:? "response_headers" .!= []
    respRoutingMetadata <- o .:? "routing_metadata" .!= HM.empty
    respAudit <- o .:? "audit" .!= emptyAudit
    respNeedsMore <- o .:? "needs_more" .!= False
    respRequestBodyMutation <- o .:? "request_body_mutation"
    respResponseBodyMutation <- o .:? "response_body_mutation"
    respWebSocketDecision <- o .:? "websocket_decision"
    pure AgentResponse {..}
    where
      emptyAudit = AuditMetadata [] [] Nothing [] HM.empty

-- | Detection severity for guardrails
data DetectionSeverity
  = SeverityLow
  | SeverityMedium
  | SeverityHigh
  | SeverityCritical
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (NFData)

instance ToJSON DetectionSeverity where
  toJSON = \case
    SeverityLow -> "low"
    SeverityMedium -> "medium"
    SeverityHigh -> "high"
    SeverityCritical -> "critical"

instance FromJSON DetectionSeverity where
  parseJSON = withText "DetectionSeverity" $ \case
    "low" -> pure SeverityLow
    "medium" -> pure SeverityMedium
    "high" -> pure SeverityHigh
    "critical" -> pure SeverityCritical
    other -> fail $ "Unknown DetectionSeverity: " <> T.unpack other

-- | Text span for guardrail detections
data TextSpan = TextSpan
  { spanStart :: !Int
  , spanEnd :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON TextSpan where
  toJSON TextSpan {..} =
    object
      [ "start" .= spanStart
      , "end" .= spanEnd
      ]

instance FromJSON TextSpan where
  parseJSON = withObject "TextSpan" $ \o -> do
    spanStart <- o .: "start"
    spanEnd <- o .: "end"
    pure TextSpan {..}

-- | Individual guardrail detection
data GuardrailDetection = GuardrailDetection
  { detCategory :: !Text
  , detDescription :: !Text
  , detSeverity :: !DetectionSeverity
  , detConfidence :: !(Maybe Double)
  , detSpan :: !(Maybe TextSpan)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON GuardrailDetection where
  toJSON GuardrailDetection {..} =
    object $
      [ "category" .= detCategory
      , "description" .= detDescription
      , "severity" .= detSeverity
      ]
        <> maybe [] (\c -> ["confidence" .= c]) detConfidence
        <> maybe [] (\s -> ["span" .= s]) detSpan

instance FromJSON GuardrailDetection where
  parseJSON = withObject "GuardrailDetection" $ \o -> do
    detCategory <- o .: "category"
    detDescription <- o .: "description"
    detSeverity <- o .:? "severity" .!= SeverityMedium
    detConfidence <- o .:? "confidence"
    detSpan <- o .:? "span"
    pure GuardrailDetection {..}

-- | Guardrail inspection response
data GuardrailResponse = GuardrailResponse
  { grRespDetected :: !Bool
  , grRespConfidence :: !Double
  , grRespDetections :: ![GuardrailDetection]
  , grRespRedactedContent :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON GuardrailResponse where
  toJSON GuardrailResponse {..} =
    object $
      [ "detected" .= grRespDetected
      , "confidence" .= grRespConfidence
      , "detections" .= grRespDetections
      ]
        <> maybe [] (\r -> ["redacted_content" .= r]) grRespRedactedContent

instance FromJSON GuardrailResponse where
  parseJSON = withObject "GuardrailResponse" $ \o -> do
    grRespDetected <- o .: "detected"
    grRespConfidence <- o .: "confidence"
    grRespDetections <- o .:? "detections" .!= []
    grRespRedactedContent <- o .:? "redacted_content"
    pure GuardrailResponse {..}

--------------------------------------------------------------------------------
-- Health Types
--------------------------------------------------------------------------------

-- | Load metrics for health reporting
data LoadMetrics = LoadMetrics
  { loadInFlight :: !Word32
  , loadQueueDepth :: !Word32
  , loadAvgLatencyMs :: !Float
  , loadP50LatencyMs :: !Float
  , loadP95LatencyMs :: !Float
  , loadP99LatencyMs :: !Float
  , loadRequestsProcessed :: !Word64
  , loadRequestsRejected :: !Word64
  , loadRequestsTimedOut :: !Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON LoadMetrics where
  toJSON LoadMetrics {..} =
    object
      [ "in_flight" .= loadInFlight
      , "queue_depth" .= loadQueueDepth
      , "avg_latency_ms" .= loadAvgLatencyMs
      , "p50_latency_ms" .= loadP50LatencyMs
      , "p95_latency_ms" .= loadP95LatencyMs
      , "p99_latency_ms" .= loadP99LatencyMs
      , "requests_processed" .= loadRequestsProcessed
      , "requests_rejected" .= loadRequestsRejected
      , "requests_timed_out" .= loadRequestsTimedOut
      ]

instance FromJSON LoadMetrics where
  parseJSON = withObject "LoadMetrics" $ \o -> do
    loadInFlight <- o .: "in_flight"
    loadQueueDepth <- o .: "queue_depth"
    loadAvgLatencyMs <- parseFloat =<< o .: "avg_latency_ms"
    loadP50LatencyMs <- parseFloat =<< o .: "p50_latency_ms"
    loadP95LatencyMs <- parseFloat =<< o .: "p95_latency_ms"
    loadP99LatencyMs <- parseFloat =<< o .: "p99_latency_ms"
    loadRequestsProcessed <- o .: "requests_processed"
    loadRequestsRejected <- o .: "requests_rejected"
    loadRequestsTimedOut <- o .: "requests_timed_out"
    pure LoadMetrics {..}
    where
      parseFloat :: Value -> Parser Float
      parseFloat (Number n) = pure $ toRealFloat n
      parseFloat _ = fail "Expected number"

-- | Resource metrics for health reporting
data ResourceMetrics = ResourceMetrics
  { resCpuPercent :: !(Maybe Float)
  , resMemoryBytes :: !(Maybe Word64)
  , resMemoryLimit :: !(Maybe Word64)
  , resActiveThreads :: !(Maybe Word32)
  , resOpenFds :: !(Maybe Word32)
  , resFdLimit :: !(Maybe Word32)
  , resConnections :: !(Maybe Word32)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ResourceMetrics where
  toJSON ResourceMetrics {..} =
    object $
      maybe [] (\v -> ["cpu_percent" .= v]) resCpuPercent
        <> maybe [] (\v -> ["memory_bytes" .= v]) resMemoryBytes
        <> maybe [] (\v -> ["memory_limit" .= v]) resMemoryLimit
        <> maybe [] (\v -> ["active_threads" .= v]) resActiveThreads
        <> maybe [] (\v -> ["open_fds" .= v]) resOpenFds
        <> maybe [] (\v -> ["fd_limit" .= v]) resFdLimit
        <> maybe [] (\v -> ["connections" .= v]) resConnections

instance FromJSON ResourceMetrics where
  parseJSON = withObject "ResourceMetrics" $ \o -> do
    resCpuPercent <- o .:? "cpu_percent"
    resMemoryBytes <- o .:? "memory_bytes"
    resMemoryLimit <- o .:? "memory_limit"
    resActiveThreads <- o .:? "active_threads"
    resOpenFds <- o .:? "open_fds"
    resFdLimit <- o .:? "fd_limit"
    resConnections <- o .:? "connections"
    pure ResourceMetrics {..}

-- | Health state of the agent
data HealthState
  = StateHealthy
  | StateDegraded
      { degradedDisabledFeatures :: ![Text]
      , degradedTimeoutMultiplier :: !Float
      }
  | StateDraining
      { drainingEtaMs :: !(Maybe Word64)
      }
  | StateUnhealthy
      { unhealthyReason :: !Text
      , unhealthyRecoverable :: !Bool
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON HealthState where
  toJSON StateHealthy =
    object ["status" .= ("healthy" :: Text)]
  toJSON StateDegraded {..} =
    object
      [ "status" .= ("degraded" :: Text)
      , "disabled_features" .= degradedDisabledFeatures
      , "timeout_multiplier" .= degradedTimeoutMultiplier
      ]
  toJSON StateDraining {..} =
    object $
      ["status" .= ("draining" :: Text)]
        <> maybe [] (\e -> ["eta_ms" .= e]) drainingEtaMs
  toJSON StateUnhealthy {..} =
    object
      [ "status" .= ("unhealthy" :: Text)
      , "reason" .= unhealthyReason
      , "recoverable" .= unhealthyRecoverable
      ]

instance FromJSON HealthState where
  parseJSON = withObject "HealthState" $ \o -> do
    status <- o .: "status" :: Parser Text
    case status of
      "healthy" -> pure StateHealthy
      "degraded" -> do
        degradedDisabledFeatures <- o .:? "disabled_features" .!= []
        degradedTimeoutMultiplier <- o .:? "timeout_multiplier" .!= 1.0
        pure StateDegraded {..}
      "draining" -> do
        drainingEtaMs <- o .:? "eta_ms"
        pure StateDraining {..}
      "unhealthy" -> do
        unhealthyReason <- o .: "reason"
        unhealthyRecoverable <- o .:? "recoverable" .!= False
        pure StateUnhealthy {..}
      other -> fail $ "Unknown HealthState: " <> T.unpack other

-- | Health status report
data HealthStatus = HealthStatus
  { hsAgentId :: !Text
  , hsState :: !HealthState
  , hsMessage :: !(Maybe Text)
  , hsLoad :: !(Maybe LoadMetrics)
  , hsResources :: !(Maybe ResourceMetrics)
  , hsValidUntilMs :: !(Maybe Word64)
  , hsTimestampMs :: !Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON HealthStatus where
  toJSON HealthStatus {..} =
    object $
      [ "agent_id" .= hsAgentId
      , "state" .= hsState
      , "timestamp_ms" .= hsTimestampMs
      ]
        <> maybe [] (\m -> ["message" .= m]) hsMessage
        <> maybe [] (\l -> ["load" .= l]) hsLoad
        <> maybe [] (\r -> ["resources" .= r]) hsResources
        <> maybe [] (\v -> ["valid_until_ms" .= v]) hsValidUntilMs

instance FromJSON HealthStatus where
  parseJSON = withObject "HealthStatus" $ \o -> do
    hsAgentId <- o .: "agent_id"
    hsState <- o .: "state"
    hsMessage <- o .:? "message"
    hsLoad <- o .:? "load"
    hsResources <- o .:? "resources"
    hsValidUntilMs <- o .:? "valid_until_ms"
    hsTimestampMs <- o .: "timestamp_ms"
    pure HealthStatus {..}

--------------------------------------------------------------------------------
-- Metrics Types
--------------------------------------------------------------------------------

-- | Counter metric
data CounterMetric = CounterMetric
  { ctrName :: !Text
  , ctrHelp :: !(Maybe Text)
  , ctrLabels :: !(HashMap Text Text)
  , ctrValue :: !Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON CounterMetric where
  toJSON CounterMetric {..} =
    object $
      [ "name" .= ctrName
      , "labels" .= ctrLabels
      , "value" .= ctrValue
      ]
        <> maybe [] (\h -> ["help" .= h]) ctrHelp

instance FromJSON CounterMetric where
  parseJSON = withObject "CounterMetric" $ \o -> do
    ctrName <- o .: "name"
    ctrHelp <- o .:? "help"
    ctrLabels <- o .:? "labels" .!= HM.empty
    ctrValue <- o .: "value"
    pure CounterMetric {..}

-- | Gauge metric
data GaugeMetric = GaugeMetric
  { gaugeName :: !Text
  , gaugeHelp :: !(Maybe Text)
  , gaugeLabels :: !(HashMap Text Text)
  , gaugeValue :: !Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON GaugeMetric where
  toJSON GaugeMetric {..} =
    object $
      [ "name" .= gaugeName
      , "labels" .= gaugeLabels
      , "value" .= gaugeValue
      ]
        <> maybe [] (\h -> ["help" .= h]) gaugeHelp

instance FromJSON GaugeMetric where
  parseJSON = withObject "GaugeMetric" $ \o -> do
    gaugeName <- o .: "name"
    gaugeHelp <- o .:? "help"
    gaugeLabels <- o .:? "labels" .!= HM.empty
    gaugeValue <- o .: "value"
    pure GaugeMetric {..}

-- | Histogram bucket
data HistogramBucket = HistogramBucket
  { bucketLe :: !Double  -- Upper bound (+Inf for last bucket)
  , bucketCount :: !Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON HistogramBucket where
  toJSON HistogramBucket {..} =
    object
      [ "le" .= if isInfinite bucketLe then "+Inf" else toJSON bucketLe
      , "count" .= bucketCount
      ]

instance FromJSON HistogramBucket where
  parseJSON = withObject "HistogramBucket" $ \o -> do
    leVal <- o .: "le"
    bucketLe <- case leVal of
      String "+Inf" -> pure (1 / 0)  -- Positive infinity
      Number n -> pure $ toRealFloat n
      _ -> fail "Expected number or \"+Inf\""
    bucketCount <- o .: "count"
    pure HistogramBucket {..}

-- | Histogram metric
data HistogramMetric = HistogramMetric
  { histName :: !Text
  , histHelp :: !(Maybe Text)
  , histLabels :: !(HashMap Text Text)
  , histSum :: !Double
  , histCount :: !Word64
  , histBuckets :: ![HistogramBucket]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON HistogramMetric where
  toJSON HistogramMetric {..} =
    object $
      [ "name" .= histName
      , "labels" .= histLabels
      , "sum" .= histSum
      , "count" .= histCount
      , "buckets" .= histBuckets
      ]
        <> maybe [] (\h -> ["help" .= h]) histHelp

instance FromJSON HistogramMetric where
  parseJSON = withObject "HistogramMetric" $ \o -> do
    histName <- o .: "name"
    histHelp <- o .:? "help"
    histLabels <- o .:? "labels" .!= HM.empty
    histSum <- o .: "sum"
    histCount <- o .: "count"
    histBuckets <- o .: "buckets"
    pure HistogramMetric {..}

-- | Metrics report
data MetricsReport = MetricsReport
  { mrAgentId :: !Text
  , mrTimestampMs :: !Word64
  , mrIntervalMs :: !Word64
  , mrCounters :: ![CounterMetric]
  , mrGauges :: ![GaugeMetric]
  , mrHistograms :: ![HistogramMetric]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON MetricsReport where
  toJSON MetricsReport {..} =
    object
      [ "agent_id" .= mrAgentId
      , "timestamp_ms" .= mrTimestampMs
      , "interval_ms" .= mrIntervalMs
      , "counters" .= mrCounters
      , "gauges" .= mrGauges
      , "histograms" .= mrHistograms
      ]

instance FromJSON MetricsReport where
  parseJSON = withObject "MetricsReport" $ \o -> do
    mrAgentId <- o .: "agent_id"
    mrTimestampMs <- o .: "timestamp_ms"
    mrIntervalMs <- o .: "interval_ms"
    mrCounters <- o .:? "counters" .!= []
    mrGauges <- o .:? "gauges" .!= []
    mrHistograms <- o .:? "histograms" .!= []
    pure MetricsReport {..}

-- | Standard metric names
metricRequestsTotal :: Text
metricRequestsTotal = "agent_requests_total"

metricRequestsBlockedTotal :: Text
metricRequestsBlockedTotal = "agent_requests_blocked_total"

metricRequestsDurationSeconds :: Text
metricRequestsDurationSeconds = "agent_requests_duration_seconds"

metricErrorsTotal :: Text
metricErrorsTotal = "agent_errors_total"

metricInFlightRequests :: Text
metricInFlightRequests = "agent_in_flight_requests"

metricQueueDepth :: Text
metricQueueDepth = "agent_queue_depth"

--------------------------------------------------------------------------------
-- Control Plane Types
--------------------------------------------------------------------------------

-- | Reason for request cancellation
data CancelReason
  = CancelClientDisconnect
  | CancelTimeout
  | CancelBlockedByAgent { cancelAgentId :: !Text }
  | CancelUpstreamError
  | CancelProxyShutdown
  | CancelManual { cancelReason :: !Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON CancelReason where
  toJSON CancelClientDisconnect =
    object ["type" .= ("client_disconnect" :: Text)]
  toJSON CancelTimeout =
    object ["type" .= ("timeout" :: Text)]
  toJSON CancelBlockedByAgent {..} =
    object ["type" .= ("blocked_by_agent" :: Text), "agent_id" .= cancelAgentId]
  toJSON CancelUpstreamError =
    object ["type" .= ("upstream_error" :: Text)]
  toJSON CancelProxyShutdown =
    object ["type" .= ("proxy_shutdown" :: Text)]
  toJSON CancelManual {..} =
    object ["type" .= ("manual" :: Text), "reason" .= cancelReason]

instance FromJSON CancelReason where
  parseJSON = withObject "CancelReason" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "client_disconnect" -> pure CancelClientDisconnect
      "timeout" -> pure CancelTimeout
      "blocked_by_agent" -> CancelBlockedByAgent <$> o .: "agent_id"
      "upstream_error" -> pure CancelUpstreamError
      "proxy_shutdown" -> pure CancelProxyShutdown
      "manual" -> CancelManual <$> o .: "reason"
      other -> fail $ "Unknown CancelReason type: " <> T.unpack other

-- | Request cancellation message
data CancelRequest = CancelRequest
  { cancelCorrelationId :: !Text
  , cancelReasonVal :: !CancelReason
  , cancelTimestampMs :: !Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON CancelRequest where
  toJSON CancelRequest {..} =
    object
      [ "correlation_id" .= cancelCorrelationId
      , "reason" .= cancelReasonVal
      , "timestamp_ms" .= cancelTimestampMs
      ]

instance FromJSON CancelRequest where
  parseJSON = withObject "CancelRequest" $ \o -> do
    cancelCorrelationId <- o .: "correlation_id"
    cancelReasonVal <- o .: "reason"
    cancelTimestampMs <- o .: "timestamp_ms"
    pure CancelRequest {..}

-- | Rule definition for config updates
data RuleDefinition = RuleDefinition
  { ruleId :: !Text
  , ruleDefinition :: !Value
  , rulePriority :: !(Maybe Int)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON RuleDefinition where
  toJSON RuleDefinition {..} =
    object $
      [ "id" .= ruleId
      , "definition" .= ruleDefinition
      ]
        <> maybe [] (\p -> ["priority" .= p]) rulePriority

instance FromJSON RuleDefinition where
  parseJSON = withObject "RuleDefinition" $ \o -> do
    ruleId <- o .: "id"
    ruleDefinition <- o .: "definition"
    rulePriority <- o .:? "priority"
    pure RuleDefinition {..}

-- | Type of configuration update
data ConfigUpdateType
  = ConfigRequestReload
  | ConfigRuleUpdate
      { cfgRuleSet :: !Text
      , cfgRules :: ![RuleDefinition]
      , cfgRemoveRules :: ![Text]
      }
  | ConfigListUpdate
      { cfgListId :: !Text
      , cfgAddItems :: ![Text]
      , cfgRemoveItems :: ![Text]
      }
  | ConfigRestartRequired
      { cfgRestartReason :: !Text
      , cfgGracePeriodMs :: !Word64
      }
  | ConfigError
      { cfgError :: !Text
      , cfgField :: !(Maybe Text)
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ConfigUpdateType where
  toJSON ConfigRequestReload =
    object ["type" .= ("request_reload" :: Text)]
  toJSON ConfigRuleUpdate {..} =
    object
      [ "type" .= ("rule_update" :: Text)
      , "rule_set" .= cfgRuleSet
      , "rules" .= cfgRules
      , "remove_rules" .= cfgRemoveRules
      ]
  toJSON ConfigListUpdate {..} =
    object
      [ "type" .= ("list_update" :: Text)
      , "list_id" .= cfgListId
      , "add" .= cfgAddItems
      , "remove" .= cfgRemoveItems
      ]
  toJSON ConfigRestartRequired {..} =
    object
      [ "type" .= ("restart_required" :: Text)
      , "reason" .= cfgRestartReason
      , "grace_period_ms" .= cfgGracePeriodMs
      ]
  toJSON ConfigError {..} =
    object $
      [ "type" .= ("config_error" :: Text)
      , "error" .= cfgError
      ]
        <> maybe [] (\f -> ["field" .= f]) cfgField

instance FromJSON ConfigUpdateType where
  parseJSON = withObject "ConfigUpdateType" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "request_reload" -> pure ConfigRequestReload
      "rule_update" -> do
        cfgRuleSet <- o .: "rule_set"
        cfgRules <- o .: "rules"
        cfgRemoveRules <- o .:? "remove_rules" .!= []
        pure ConfigRuleUpdate {..}
      "list_update" -> do
        cfgListId <- o .: "list_id"
        cfgAddItems <- o .:? "add" .!= []
        cfgRemoveItems <- o .:? "remove" .!= []
        pure ConfigListUpdate {..}
      "restart_required" -> do
        cfgRestartReason <- o .: "reason"
        cfgGracePeriodMs <- o .: "grace_period_ms"
        pure ConfigRestartRequired {..}
      "config_error" -> do
        cfgError <- o .: "error"
        cfgField <- o .:? "field"
        pure ConfigError {..}
      other -> fail $ "Unknown ConfigUpdateType: " <> T.unpack other

-- | Configuration update request
data ConfigUpdateRequest = ConfigUpdateRequest
  { cfgUpdateType :: !ConfigUpdateType
  , cfgRequestId :: !Text
  , cfgTimestampMs :: !Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ConfigUpdateRequest where
  toJSON ConfigUpdateRequest {..} =
    object
      [ "update_type" .= cfgUpdateType
      , "request_id" .= cfgRequestId
      , "timestamp_ms" .= cfgTimestampMs
      ]

instance FromJSON ConfigUpdateRequest where
  parseJSON = withObject "ConfigUpdateRequest" $ \o -> do
    cfgUpdateType <- o .: "update_type"
    cfgRequestId <- o .: "request_id"
    cfgTimestampMs <- o .: "timestamp_ms"
    pure ConfigUpdateRequest {..}

-- | Configuration update response
data ConfigUpdateResponse = ConfigUpdateResponse
  { cfgRespRequestId :: !Text
  , cfgRespAccepted :: !Bool
  , cfgRespError :: !(Maybe Text)
  , cfgRespTimestampMs :: !Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ConfigUpdateResponse where
  toJSON ConfigUpdateResponse {..} =
    object $
      [ "request_id" .= cfgRespRequestId
      , "accepted" .= cfgRespAccepted
      , "timestamp_ms" .= cfgRespTimestampMs
      ]
        <> maybe [] (\e -> ["error" .= e]) cfgRespError

instance FromJSON ConfigUpdateResponse where
  parseJSON = withObject "ConfigUpdateResponse" $ \o -> do
    cfgRespRequestId <- o .: "request_id"
    cfgRespAccepted <- o .: "accepted"
    cfgRespError <- o .:? "error"
    cfgRespTimestampMs <- o .: "timestamp_ms"
    pure ConfigUpdateResponse {..}

-- | Shutdown reason
data ShutdownReason
  = ShutdownGraceful
  | ShutdownImmediate
  | ShutdownRestart
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (NFData)

instance ToJSON ShutdownReason where
  toJSON = \case
    ShutdownGraceful -> "graceful"
    ShutdownImmediate -> "immediate"
    ShutdownRestart -> "restart"

instance FromJSON ShutdownReason where
  parseJSON = withText "ShutdownReason" $ \case
    "graceful" -> pure ShutdownGraceful
    "immediate" -> pure ShutdownImmediate
    "restart" -> pure ShutdownRestart
    other -> fail $ "Unknown ShutdownReason: " <> T.unpack other

-- | Drain reason
data DrainReason
  = DrainMaintenance
  | DrainScaleDown
  | DrainRebalance
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (NFData)

instance ToJSON DrainReason where
  toJSON = \case
    DrainMaintenance -> "maintenance"
    DrainScaleDown -> "scale_down"
    DrainRebalance -> "rebalance"

instance FromJSON DrainReason where
  parseJSON = withText "DrainReason" $ \case
    "maintenance" -> pure DrainMaintenance
    "scale_down" -> pure DrainScaleDown
    "rebalance" -> pure DrainRebalance
    other -> fail $ "Unknown DrainReason: " <> T.unpack other

-- | Log level
data LogLevel
  = LogDebug
  | LogInfo
  | LogWarn
  | LogError
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (NFData)

instance ToJSON LogLevel where
  toJSON = \case
    LogDebug -> "debug"
    LogInfo -> "info"
    LogWarn -> "warn"
    LogError -> "error"

instance FromJSON LogLevel where
  parseJSON = withText "LogLevel" $ \case
    "debug" -> pure LogDebug
    "info" -> pure LogInfo
    "warn" -> pure LogWarn
    "error" -> pure LogError
    other -> fail $ "Unknown LogLevel: " <> T.unpack other

--------------------------------------------------------------------------------
-- Flow Control
--------------------------------------------------------------------------------

-- | Flow control action
data FlowAction
  = FlowPause
  | FlowResume
  | FlowUpdateCapacity { flowBufferAvailable :: !Int }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON FlowAction where
  toJSON FlowPause =
    object ["type" .= ("pause" :: Text)]
  toJSON FlowResume =
    object ["type" .= ("resume" :: Text)]
  toJSON FlowUpdateCapacity {..} =
    object
      [ "type" .= ("update_capacity" :: Text)
      , "buffer_available" .= flowBufferAvailable
      ]

instance FromJSON FlowAction where
  parseJSON = withObject "FlowAction" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "pause" -> pure FlowPause
      "resume" -> pure FlowResume
      "update_capacity" -> FlowUpdateCapacity <$> o .: "buffer_available"
      other -> fail $ "Unknown FlowAction type: " <> T.unpack other

-- | Flow control signal
data FlowControlSignal = FlowControlSignal
  { flowCorrelationId :: !(Maybe Text)
  , flowAction :: !FlowAction
  , flowTimestampMs :: !Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON FlowControlSignal where
  toJSON FlowControlSignal {..} =
    object $
      [ "action" .= flowAction
      , "timestamp_ms" .= flowTimestampMs
      ]
        <> maybe [] (\c -> ["correlation_id" .= c]) flowCorrelationId

instance FromJSON FlowControlSignal where
  parseJSON = withObject "FlowControlSignal" $ \o -> do
    flowCorrelationId <- o .:? "correlation_id"
    flowAction <- o .: "action"
    flowTimestampMs <- o .: "timestamp_ms"
    pure FlowControlSignal {..}

-- | Stream state
data StreamState
  = StreamDisconnected
  | StreamHandshaking
  | StreamActive
  | StreamPaused
  | StreamDraining
  | StreamClosed
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (NFData)

instance ToJSON StreamState where
  toJSON = \case
    StreamDisconnected -> "disconnected"
    StreamHandshaking -> "handshaking"
    StreamActive -> "active"
    StreamPaused -> "paused"
    StreamDraining -> "draining"
    StreamClosed -> "closed"

instance FromJSON StreamState where
  parseJSON = withText "StreamState" $ \case
    "disconnected" -> pure StreamDisconnected
    "handshaking" -> pure StreamHandshaking
    "active" -> pure StreamActive
    "paused" -> pure StreamPaused
    "draining" -> pure StreamDraining
    "closed" -> pure StreamClosed
    other -> fail $ "Unknown StreamState: " <> T.unpack other

--------------------------------------------------------------------------------
-- Envelope Types
--------------------------------------------------------------------------------

-- | Message from agent to proxy
data AgentMessage
  = AgentMsgHandshakeResponse !HandshakeResponse
  | AgentMsgResponse !AgentResponse
  | AgentMsgGuardrailResponse !GuardrailResponse
  | AgentMsgHealthStatus !HealthStatus
  | AgentMsgMetricsReport !MetricsReport
  | AgentMsgConfigResponse !ConfigUpdateResponse
  | AgentMsgFlowControl !FlowControlSignal
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON AgentMessage where
  toJSON (AgentMsgHandshakeResponse r) =
    object ["type" .= ("handshake_response" :: Text), "payload" .= r]
  toJSON (AgentMsgResponse r) =
    object ["type" .= ("response" :: Text), "payload" .= r]
  toJSON (AgentMsgGuardrailResponse r) =
    object ["type" .= ("guardrail_response" :: Text), "payload" .= r]
  toJSON (AgentMsgHealthStatus s) =
    object ["type" .= ("health_status" :: Text), "payload" .= s]
  toJSON (AgentMsgMetricsReport r) =
    object ["type" .= ("metrics_report" :: Text), "payload" .= r]
  toJSON (AgentMsgConfigResponse r) =
    object ["type" .= ("config_response" :: Text), "payload" .= r]
  toJSON (AgentMsgFlowControl s) =
    object ["type" .= ("flow_control" :: Text), "payload" .= s]

instance FromJSON AgentMessage where
  parseJSON = withObject "AgentMessage" $ \o -> do
    typ <- o .: "type" :: Parser Text
    payload <- o .: "payload"
    case typ of
      "handshake_response" -> AgentMsgHandshakeResponse <$> parseJSON payload
      "response" -> AgentMsgResponse <$> parseJSON payload
      "guardrail_response" -> AgentMsgGuardrailResponse <$> parseJSON payload
      "health_status" -> AgentMsgHealthStatus <$> parseJSON payload
      "metrics_report" -> AgentMsgMetricsReport <$> parseJSON payload
      "config_response" -> AgentMsgConfigResponse <$> parseJSON payload
      "flow_control" -> AgentMsgFlowControl <$> parseJSON payload
      other -> fail $ "Unknown AgentMessage type: " <> T.unpack other

-- | Message from proxy to agent
data ProxyMessage
  = ProxyMsgHandshake !HandshakeRequest
  | ProxyMsgRequestHeaders !RequestHeadersEvent
  | ProxyMsgRequestBodyChunk !RequestBodyChunkEvent
  | ProxyMsgResponseHeaders !ResponseHeadersEvent
  | ProxyMsgResponseBodyChunk !ResponseBodyChunkEvent
  | ProxyMsgRequestComplete !RequestCompleteEvent
  | ProxyMsgWebSocketFrame !WebSocketFrameEvent
  | ProxyMsgGuardrailInspect !GuardrailInspectEvent
  | ProxyMsgCancel !CancelRequest
  | ProxyMsgConfigUpdate !ConfigUpdateRequest
  | ProxyMsgFlowControl !FlowControlSignal
  | ProxyMsgShutdown !ShutdownReason !Word64
  | ProxyMsgDrain !Word64 !DrainReason
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ProxyMessage where
  toJSON (ProxyMsgHandshake r) =
    object ["type" .= ("handshake" :: Text), "payload" .= r]
  toJSON (ProxyMsgRequestHeaders e) =
    object ["type" .= ("request_headers" :: Text), "payload" .= e]
  toJSON (ProxyMsgRequestBodyChunk e) =
    object ["type" .= ("request_body_chunk" :: Text), "payload" .= e]
  toJSON (ProxyMsgResponseHeaders e) =
    object ["type" .= ("response_headers" :: Text), "payload" .= e]
  toJSON (ProxyMsgResponseBodyChunk e) =
    object ["type" .= ("response_body_chunk" :: Text), "payload" .= e]
  toJSON (ProxyMsgRequestComplete e) =
    object ["type" .= ("request_complete" :: Text), "payload" .= e]
  toJSON (ProxyMsgWebSocketFrame e) =
    object ["type" .= ("websocket_frame" :: Text), "payload" .= e]
  toJSON (ProxyMsgGuardrailInspect e) =
    object ["type" .= ("guardrail_inspect" :: Text), "payload" .= e]
  toJSON (ProxyMsgCancel r) =
    object ["type" .= ("cancel" :: Text), "payload" .= r]
  toJSON (ProxyMsgConfigUpdate r) =
    object ["type" .= ("config_update" :: Text), "payload" .= r]
  toJSON (ProxyMsgFlowControl s) =
    object ["type" .= ("flow_control" :: Text), "payload" .= s]
  toJSON (ProxyMsgShutdown reason timeout) =
    object
      [ "type" .= ("shutdown" :: Text)
      , "payload" .= object ["reason" .= reason, "timeout_ms" .= timeout]
      ]
  toJSON (ProxyMsgDrain timeout reason) =
    object
      [ "type" .= ("drain" :: Text)
      , "payload" .= object ["timeout_ms" .= timeout, "reason" .= reason]
      ]

instance FromJSON ProxyMessage where
  parseJSON = withObject "ProxyMessage" $ \o -> do
    typ <- o .: "type" :: Parser Text
    payload <- o .: "payload"
    case typ of
      "handshake" -> ProxyMsgHandshake <$> parseJSON payload
      "request_headers" -> ProxyMsgRequestHeaders <$> parseJSON payload
      "request_body_chunk" -> ProxyMsgRequestBodyChunk <$> parseJSON payload
      "response_headers" -> ProxyMsgResponseHeaders <$> parseJSON payload
      "response_body_chunk" -> ProxyMsgResponseBodyChunk <$> parseJSON payload
      "request_complete" -> ProxyMsgRequestComplete <$> parseJSON payload
      "websocket_frame" -> ProxyMsgWebSocketFrame <$> parseJSON payload
      "guardrail_inspect" -> ProxyMsgGuardrailInspect <$> parseJSON payload
      "cancel" -> ProxyMsgCancel <$> parseJSON payload
      "config_update" -> ProxyMsgConfigUpdate <$> parseJSON payload
      "flow_control" -> ProxyMsgFlowControl <$> parseJSON payload
      "shutdown" -> do
        inner <- parseJSON payload
        ProxyMsgShutdown
          <$> inner .: "reason"
          <*> inner .: "timeout_ms"
      "drain" -> do
        inner <- parseJSON payload
        ProxyMsgDrain
          <$> inner .: "timeout_ms"
          <*> inner .: "reason"
      other -> fail $ "Unknown ProxyMessage type: " <> T.unpack other

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Create an Allow response
allow :: AgentResponse
allow =
  AgentResponse
    { respVersion = 1
    , respDecision = Allow
    , respRequestHeaders = []
    , respResponseHeaders = []
    , respRoutingMetadata = HM.empty
    , respAudit = AuditMetadata [] [] Nothing [] HM.empty
    , respNeedsMore = False
    , respRequestBodyMutation = Nothing
    , respResponseBodyMutation = Nothing
    , respWebSocketDecision = Nothing
    }

-- | Create a Block response
block :: Word16 -> Text -> AgentResponse
block status body =
  allow {respDecision = Block status (Just body) Nothing}

-- | Create a Redirect response
redirect :: Text -> Word16 -> AgentResponse
redirect url status =
  allow {respDecision = Redirect url status}

-- | Create a Challenge response
challenge :: Text -> HashMap Text Text -> AgentResponse
challenge typ params =
  allow {respDecision = Challenge typ params}

-- | Create a healthy status
healthy :: Text -> Word64 -> HealthStatus
healthy agentId timestamp =
  HealthStatus
    { hsAgentId = agentId
    , hsState = StateHealthy
    , hsMessage = Nothing
    , hsLoad = Nothing
    , hsResources = Nothing
    , hsValidUntilMs = Nothing
    , hsTimestampMs = timestamp
    }

-- | Create a degraded status
degraded :: Text -> [Text] -> Float -> Word64 -> HealthStatus
degraded agentId features mult timestamp =
  HealthStatus
    { hsAgentId = agentId
    , hsState = StateDegraded features mult
    , hsMessage = Nothing
    , hsLoad = Nothing
    , hsResources = Nothing
    , hsValidUntilMs = Nothing
    , hsTimestampMs = timestamp
    }

-- | Create a draining status
draining :: Text -> Maybe Word64 -> Word64 -> HealthStatus
draining agentId eta timestamp =
  HealthStatus
    { hsAgentId = agentId
    , hsState = StateDraining eta
    , hsMessage = Nothing
    , hsLoad = Nothing
    , hsResources = Nothing
    , hsValidUntilMs = Nothing
    , hsTimestampMs = timestamp
    }

-- | Create an unhealthy status
unhealthy :: Text -> Text -> Bool -> Word64 -> HealthStatus
unhealthy agentId reason recoverable timestamp =
  HealthStatus
    { hsAgentId = agentId
    , hsState = StateUnhealthy reason recoverable
    , hsMessage = Nothing
    , hsLoad = Nothing
    , hsResources = Nothing
    , hsValidUntilMs = Nothing
    , hsTimestampMs = timestamp
    }

-- | Create a counter metric
counter :: Text -> Word64 -> CounterMetric
counter name value =
  CounterMetric
    { ctrName = name
    , ctrHelp = Nothing
    , ctrLabels = HM.empty
    , ctrValue = value
    }

-- | Create a gauge metric
gauge :: Text -> Double -> GaugeMetric
gauge name value =
  GaugeMetric
    { gaugeName = name
    , gaugeHelp = Nothing
    , gaugeLabels = HM.empty
    , gaugeValue = value
    }

-- | Create a histogram metric
histogram :: Text -> Double -> Word64 -> [HistogramBucket] -> HistogramMetric
histogram name sumVal count buckets =
  HistogramMetric
    { histName = name
    , histHelp = Nothing
    , histLabels = HM.empty
    , histSum = sumVal
    , histCount = count
    , histBuckets = buckets
    }
