{-# LANGUAGE DefaultSignatures #-}

-- |
-- Module      : Sentinel.Agent.Handler
-- Description : AgentHandler typeclass for implementing agent logic
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : experimental
--
-- This module defines the 'AgentHandler' typeclass that users implement to
-- create Sentinel agents. The typeclass uses 'MonadUnliftIO' to support safe
-- async operations like timeouts and concurrent processing.
--
-- = Quick Start
--
-- For simple agents, you can implement the handler directly for 'IO':
--
-- @
-- instance AgentHandler IO where
--   capabilities = pure $ defaultCapabilities "my-agent"
--   onRequestHeaders _ = pure allow
-- @
--
-- For agents with state, use 'ReaderT' with your environment:
--
-- @
-- data MyAgent = MyAgent
--   { counter :: IORef Int
--   }
--
-- type AgentM = ReaderT MyAgent IO
--
-- instance AgentHandler AgentM where
--   capabilities = pure $ defaultCapabilities "stateful-agent"
--   onRequestHeaders event = do
--     agent <- ask
--     count <- liftIO $ atomicModifyIORef' agent.counter (\\n -> (n + 1, n + 1))
--     pure allow
-- @
module Sentinel.Agent.Handler
  ( -- * Agent Handler Typeclass
    AgentHandler (..)

    -- * Convenience Types
  , Agent

    -- * Default Response Helpers
  , defaultAllow
  , defaultHealthy
  ) where

import Control.Monad.Reader (ReaderT)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Word (Word64)
import Sentinel.Agent.Types
import UnliftIO (MonadUnliftIO)

-- | Type alias for agents using ReaderT pattern
--
-- This is the recommended pattern for stateful agents:
--
-- @
-- data MyEnv = MyEnv { config :: Config, metrics :: MetricsRef }
-- type MyAgent = Agent MyEnv
-- @
type Agent env = ReaderT env IO

-- | Agent handler typeclass for implementing policy logic.
--
-- Uses 'MonadUnliftIO' constraint for safe async operations (timeouts, concurrency).
-- This allows agents to use 'UnliftIO.timeout', 'UnliftIO.async', and other
-- concurrent primitives safely.
--
-- = Minimal Implementation
--
-- The minimal implementation requires 'capabilities' and 'onRequestHeaders':
--
-- @
-- instance AgentHandler IO where
--   capabilities = pure $ defaultCapabilities "my-agent"
--   onRequestHeaders _ = pure allow
-- @
--
-- = Event Handling
--
-- Events are processed in the order they arrive from the proxy:
--
-- 1. 'onRequestHeaders' - Called first for every request (required)
-- 2. 'onRequestBodyChunk' - Called for each body chunk (if streaming enabled)
-- 3. 'onResponseHeaders' - Called when upstream responds
-- 4. 'onResponseBodyChunk' - Called for each response body chunk
-- 5. 'onRequestComplete' - Called after request/response is complete
--
-- For WebSocket connections:
--
-- * 'onWebSocketFrame' - Called for each WebSocket frame
--
-- For guardrail inspection:
--
-- * 'onGuardrailInspect' - Called for content inspection requests
class MonadUnliftIO m => AgentHandler m where
  -- | Report agent capabilities.
  --
  -- Called once at startup during handshake with the proxy.
  -- Must return the agent's capabilities including supported events,
  -- features, limits, and health configuration.
  --
  -- @
  -- capabilities = pure AgentCapabilities
  --   { capProtocolVersion = 2
  --   , capAgentId = "waf-001"
  --   , capName = "waf-agent"
  --   , capVersion = "1.0.0"
  --   , capSupportedEvents = [RequestHeaders, RequestBodyChunk]
  --   , capFeatures = defaultFeatures { featStreamingBody = True }
  --   , capLimits = defaultLimits
  --   , capHealthConfig = defaultHealthConfig
  --   }
  -- @
  capabilities :: m AgentCapabilities

  -- | Handle request headers event.
  --
  -- This is the main policy entry point, called for every incoming request.
  -- Must be implemented by all agents.
  --
  -- Return an 'AgentResponse' with:
  --
  -- * 'Allow' to continue to upstream
  -- * 'Block' to return an error response to the client
  -- * 'Redirect' to redirect the client
  -- * 'Challenge' to issue a challenge (e.g., CAPTCHA)
  --
  -- You can also modify headers via 'respRequestHeaders' and 'respResponseHeaders'.
  --
  -- @
  -- onRequestHeaders event = do
  --   if isBlocked event.reqHdrUri
  --     then pure $ block 403 "Forbidden"
  --     else pure allow
  -- @
  onRequestHeaders :: RequestHeadersEvent -> m AgentResponse

  -- | Handle request body chunk event.
  --
  -- Called for each chunk of the request body when streaming is enabled.
  -- Default implementation allows all chunks to pass through.
  --
  -- Set 'respNeedsMore = True' in the response to request more chunks
  -- before making a final decision.
  --
  -- @
  -- onRequestBodyChunk event = do
  --   if containsMalicious event.reqBodyData
  --     then pure $ block 400 "Bad Request"
  --     else pure allow { respNeedsMore = not event.reqBodyIsLast }
  -- @
  onRequestBodyChunk :: RequestBodyChunkEvent -> m AgentResponse
  onRequestBodyChunk _ = pure defaultAllow

  -- | Handle response headers event.
  --
  -- Called when response headers are received from upstream.
  -- Default implementation allows all responses.
  --
  -- Useful for:
  --
  -- * Adding security headers
  -- * Modifying cache headers
  -- * Blocking certain responses
  --
  -- @
  -- onResponseHeaders event = do
  --   let addHeaders =
  --         [ HeaderSet "X-Content-Type-Options" "nosniff"
  --         , HeaderSet "X-Frame-Options" "DENY"
  --         ]
  --   pure allow { respResponseHeaders = addHeaders }
  -- @
  onResponseHeaders :: ResponseHeadersEvent -> m AgentResponse
  onResponseHeaders _ = pure defaultAllow

  -- | Handle response body chunk event.
  --
  -- Called for each chunk of the response body when streaming is enabled.
  -- Default implementation allows all chunks.
  --
  -- Can be used for:
  --
  -- * Content scanning
  -- * Response modification
  -- * Data loss prevention
  onResponseBodyChunk :: ResponseBodyChunkEvent -> m AgentResponse
  onResponseBodyChunk _ = pure defaultAllow

  -- | Handle WebSocket frame event.
  --
  -- Called for each WebSocket frame when WebSocket support is enabled.
  -- Default implementation allows all frames.
  --
  -- Return a response with 'respWebSocketDecision' set to:
  --
  -- * 'WsAllow' - Allow the frame through
  -- * 'WsDrop' - Silently drop the frame
  -- * 'WsClose' - Close the WebSocket connection
  --
  -- @
  -- onWebSocketFrame event = do
  --   if isValidFrame event
  --     then pure allow { respWebSocketDecision = Just WsAllow }
  --     else pure allow { respWebSocketDecision = Just (WsClose 1008 "Policy violation") }
  -- @
  onWebSocketFrame :: WebSocketFrameEvent -> m AgentResponse
  onWebSocketFrame _ = pure defaultAllow

  -- | Handle guardrail inspection event.
  --
  -- Called for content inspection requests (prompt injection, PII detection).
  -- Default implementation returns no detections.
  --
  -- @
  -- onGuardrailInspect event = do
  --   detections <- runDetection event.grInspectType event.grInspectContent
  --   pure GuardrailResponse
  --     { grRespDetected = not (null detections)
  --     , grRespConfidence = maximum (map detConfidence detections)
  --     , grRespDetections = detections
  --     , grRespRedactedContent = Nothing
  --     }
  -- @
  onGuardrailInspect :: GuardrailInspectEvent -> m GuardrailResponse
  onGuardrailInspect _ =
    pure
      GuardrailResponse
        { grRespDetected = False
        , grRespConfidence = 0.0
        , grRespDetections = []
        , grRespRedactedContent = Nothing
        }

  -- | Handle request complete event.
  --
  -- Called after the request/response cycle is complete.
  -- Useful for logging and metrics collection.
  -- Default implementation does nothing.
  --
  -- @
  -- onRequestComplete event = do
  --   logRequest event.reqCompRequestId event.reqCompStatusCode event.reqCompDurationMs
  -- @
  onRequestComplete :: RequestCompleteEvent -> m ()
  onRequestComplete _ = pure ()

  -- | Report current health status.
  --
  -- Called periodically by the server to report health to the proxy.
  -- Default implementation reports healthy status.
  --
  -- Override to report degraded/unhealthy status when appropriate:
  --
  -- @
  -- healthStatus = do
  --   load <- getCurrentLoad
  --   timestamp <- getCurrentTimeMs
  --   if load > 0.9
  --     then pure $ degraded "agent-001" ["rate_limiting"] 2.0 timestamp
  --     else pure $ healthy "agent-001" timestamp
  -- @
  healthStatus :: m HealthStatus
  healthStatus = pure defaultHealthy

  -- | Report metrics.
  --
  -- Called periodically by the server to collect metrics.
  -- Default implementation returns no metrics.
  --
  -- @
  -- metricsReport = do
  --   counters <- getCounters
  --   timestamp <- getCurrentTimeMs
  --   pure $ Just MetricsReport
  --     { mrAgentId = "agent-001"
  --     , mrTimestampMs = timestamp
  --     , mrIntervalMs = 10000
  --     , mrCounters = counters
  --     , mrGauges = []
  --     , mrHistograms = []
  --     }
  -- @
  metricsReport :: m (Maybe MetricsReport)
  metricsReport = pure Nothing

  -- | Handle configuration update.
  --
  -- Called when the proxy pushes a configuration update.
  -- Return 'True' to accept the update, 'False' to reject.
  --
  -- The 'Value' parameter contains the new configuration as JSON.
  -- The 'Maybe Text' parameter contains an optional request ID.
  --
  -- @
  -- onConfigure newConfig requestId = do
  --   case fromJSON newConfig of
  --     Success cfg -> do
  --       updateConfig cfg
  --       pure True
  --     Error _ -> pure False
  -- @
  onConfigure :: Value -> Maybe Text -> m Bool
  onConfigure _ _ = pure True

  -- | Handle shutdown request.
  --
  -- Called when the proxy requests agent shutdown.
  -- Use this to clean up resources before exiting.
  --
  -- @
  -- onShutdown reason timeoutMs = do
  --   logInfo $ "Shutting down: " <> show reason
  --   flushMetrics
  --   closeConnections
  -- @
  onShutdown :: ShutdownReason -> Word64 -> m ()
  onShutdown _ _ = pure ()

  -- | Handle drain request.
  --
  -- Called when the proxy wants the agent to stop accepting new requests.
  -- Existing requests should be completed within the timeout.
  --
  -- @
  -- onDrain timeoutMs reason = do
  --   logInfo $ "Draining: " <> show reason
  --   setAcceptingRequests False
  -- @
  onDrain :: Word64 -> DrainReason -> m ()
  onDrain _ _ = pure ()

  -- | Handle request cancellation.
  --
  -- Called when a request is cancelled by the proxy.
  -- Default implementation does nothing.
  --
  -- @
  -- onCancel cancel = do
  --   logInfo $ "Request cancelled: " <> cancel.cancelCorrelationId
  --   cleanupRequest cancel.cancelCorrelationId
  -- @
  onCancel :: CancelRequest -> m ()
  onCancel _ = pure ()

-- | Default allow response
defaultAllow :: AgentResponse
defaultAllow = allow

-- | Default healthy status (uses placeholder values that should be filled by server)
defaultHealthy :: HealthStatus
defaultHealthy =
  HealthStatus
    { hsAgentId = ""
    , hsState = StateHealthy
    , hsMessage = Nothing
    , hsLoad = Nothing
    , hsResources = Nothing
    , hsValidUntilMs = Nothing
    , hsTimestampMs = 0
    }
