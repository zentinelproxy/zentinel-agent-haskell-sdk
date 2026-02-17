-- |
-- Module      : Zentinel.Agent.Protocol
-- Description : Zentinel Agent Protocol v2 SDK for Haskell
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : experimental
--
-- This is the main module for the Zentinel Agent Protocol v2 SDK.
-- It re-exports all the types and functions needed to implement
-- Zentinel agents in Haskell.
--
-- = Quick Start
--
-- @
-- module Main where
--
-- import Zentinel.Agent.Protocol
--
-- -- Simple agent that allows all requests
-- instance AgentHandler IO where
--   capabilities = pure $ defaultCapabilities "my-agent"
--   onRequestHeaders _ = pure allow
--
-- main :: IO ()
-- main = runAgent defaultConfig id
-- @
--
-- = Agent with State
--
-- For agents that need to maintain state, use the 'ReaderT' pattern:
--
-- @
-- module Main where
--
-- import Zentinel.Agent.Protocol
-- import Control.Monad.Reader (ReaderT, ask, runReaderT)
-- import Data.IORef
-- import UnliftIO (atomicModifyIORef')
--
-- data WafAgent = WafAgent
--   { blockedCount :: IORef Int
--   , allowedCount :: IORef Int
--   }
--
-- instance AgentHandler (ReaderT WafAgent IO) where
--   capabilities = pure AgentCapabilities
--     { capProtocolVersion = protocolVersion
--     , capAgentId = "waf-agent-001"
--     , capName = "waf-agent"
--     , capVersion = "0.1.0"
--     , capSupportedEvents = [RequestHeaders]
--     , capFeatures = defaultFeatures { featHealthReporting = True }
--     , capLimits = defaultLimits
--     , capHealthConfig = defaultHealthConfig
--     }
--
--   onRequestHeaders event = do
--     agent <- ask
--     if "/admin" `T.isPrefixOf` reqHdrUri event
--       then do
--         atomicModifyIORef' (blockedCount agent) (\\n -> (n + 1, ()))
--         pure $ block 403 "Forbidden"
--       else do
--         atomicModifyIORef' (allowedCount agent) (\\n -> (n + 1, ()))
--         pure allow
--
-- main :: IO ()
-- main = do
--   agent <- WafAgent <$> newIORef 0 <*> newIORef 0
--   runAgent defaultConfig (flip runReaderT agent)
-- @
--
-- = Architecture
--
-- The SDK is organized into several modules:
--
-- * "Zentinel.Agent.Types" - All protocol types (events, decisions, etc.)
-- * "Zentinel.Agent.Handler" - The 'AgentHandler' typeclass
-- * "Zentinel.Agent.Server" - Server runner and configuration
-- * "Zentinel.Agent.Health" - Health reporting utilities
-- * "Zentinel.Agent.Metrics" - Metrics collection utilities
-- * "Zentinel.Agent.Transport.UDS" - Unix socket transport
-- * "Zentinel.Agent.Transport.GRPC" - gRPC transport (placeholder)
module Zentinel.Agent.Protocol
  ( -- * Running Agents
    runAgent
  , runAgentWith
  , ServerConfig (..)
  , defaultConfig
  , LogLevel (..)
  , HostPort (..)

    -- * Handler Typeclass
  , AgentHandler (..)
  , Agent

    -- * Protocol Constants
  , protocolVersion
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
  , AuditMetadata (..)

    -- ** Response Builders
  , allow
  , block
  , redirect
  , challenge

    -- * Health
  , HealthStatus (..)
  , HealthState (..)
  , LoadMetrics (..)
  , ResourceMetrics (..)

    -- ** Health Builders
  , healthy
  , degraded
  , draining
  , unhealthy
  , healthyStatus
  , degradedStatus
  , drainingStatus
  , unhealthyStatus
  , emptyLoadMetrics
  , newLoadMetrics
  , emptyResourceMetrics
  , getCurrentTimeMs

    -- * Metrics
  , MetricsReport (..)
  , CounterMetric (..)
  , GaugeMetric (..)
  , HistogramMetric (..)
  , HistogramBucket (..)

    -- ** Metric Builders
  , counter
  , gauge
  , histogram
  , newMetricsReport
  , emptyMetricsReport
  , counterMetric
  , counterWithLabels
  , counterWithHelp
  , gaugeMetric
  , gaugeWithLabels
  , gaugeWithHelp
  , histogramMetric
  , histogramWithLabels
  , histogramBucket
  , defaultLatencyBuckets

    -- ** Standard Metric Names
  , metricRequestsTotal
  , metricRequestsBlockedTotal
  , metricRequestsDurationSeconds
  , metricErrorsTotal
  , metricInFlightRequests
  , metricQueueDepth

    -- ** Metrics Collector
  , MetricsCollector
  , newMetricsCollector
  , incrementCounter
  , setGauge
  , observeHistogram
  , collectMetrics

    -- * Control Plane
  , CancelRequest (..)
  , CancelReason (..)
  , ConfigUpdateRequest (..)
  , ConfigUpdateType (..)
  , RuleDefinition (..)
  , ConfigUpdateResponse (..)
  , ShutdownReason (..)
  , DrainReason (..)

    -- * Flow Control
  , FlowControlSignal (..)
  , FlowAction (..)
  , StreamState (..)

    -- * Message Envelopes
  , AgentMessage (..)
  , ProxyMessage (..)

    -- * Re-exports
  , module UnliftIO
  ) where

import Zentinel.Agent.Handler
import Zentinel.Agent.Health
import Zentinel.Agent.Metrics
import Zentinel.Agent.Server
import Zentinel.Agent.Types
import UnliftIO (MonadIO, MonadUnliftIO, atomicModifyIORef', liftIO, newIORef, readIORef)
