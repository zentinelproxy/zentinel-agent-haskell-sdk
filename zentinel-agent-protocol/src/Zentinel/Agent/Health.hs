-- |
-- Module      : Zentinel.Agent.Health
-- Description : Health reporting utilities for Zentinel agents
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : experimental
--
-- This module provides utilities for health reporting in Zentinel agents.
-- Agents report their health status periodically to allow the proxy to
-- make informed routing decisions.
--
-- = Health States
--
-- * 'StateHealthy' - Agent is fully operational
-- * 'StateDegraded' - Agent is operational but some features are disabled
-- * 'StateDraining' - Agent is preparing to shut down
-- * 'StateUnhealthy' - Agent cannot process requests
--
-- = Usage
--
-- @
-- instance AgentHandler MyAgent where
--   healthStatus = do
--     load <- getLoadMetrics
--     timestamp <- getCurrentTimeMs
--     if load.loadInFlight > 90
--       then pure $ degradedStatus "agent-001" ["rate_limiting"] 2.0 timestamp
--       else pure $ healthyStatus "agent-001" timestamp
-- @
module Zentinel.Agent.Health
  ( -- * Health Status Builders
    healthyStatus
  , degradedStatus
  , drainingStatus
  , unhealthyStatus

    -- * Load Metrics Helpers
  , emptyLoadMetrics
  , newLoadMetrics

    -- * Resource Metrics Helpers
  , emptyResourceMetrics

    -- * Time Utilities
  , getCurrentTimeMs
  ) where

import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32, Word64)
import Zentinel.Agent.Types
import UnliftIO (MonadIO, liftIO)

-- | Create a healthy status
--
-- @
-- status <- healthyStatus "my-agent-001" <$> getCurrentTimeMs
-- @
healthyStatus :: Text -> Word64 -> HealthStatus
healthyStatus agentId timestamp =
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
--
-- Use this when the agent is still operational but some features are disabled
-- or performance is degraded.
--
-- @
-- -- Agent is under heavy load, disable streaming
-- status <- degradedStatus "my-agent-001" ["streaming_body"] 2.0 <$> getCurrentTimeMs
-- @
degradedStatus ::
  -- | Agent ID
  Text ->
  -- | Disabled features
  [Text] ->
  -- | Timeout multiplier (e.g., 2.0 means double timeouts)
  Float ->
  -- | Timestamp in milliseconds
  Word64 ->
  HealthStatus
degradedStatus agentId features mult timestamp =
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
--
-- Use this when the agent is preparing to shut down.
-- The proxy will stop sending new requests but allow existing requests
-- to complete.
--
-- @
-- -- Draining, expect to be ready for shutdown in 30 seconds
-- status <- drainingStatus "my-agent-001" (Just 30000) <$> getCurrentTimeMs
-- @
drainingStatus ::
  -- | Agent ID
  Text ->
  -- | Estimated time until drained (milliseconds)
  Maybe Word64 ->
  -- | Timestamp in milliseconds
  Word64 ->
  HealthStatus
drainingStatus agentId eta timestamp =
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
--
-- Use this when the agent cannot process requests.
--
-- @
-- -- Database connection lost
-- status <- unhealthyStatus "my-agent-001" "Database unavailable" True <$> getCurrentTimeMs
-- @
unhealthyStatus ::
  -- | Agent ID
  Text ->
  -- | Reason for unhealthy status
  Text ->
  -- | Is the condition recoverable?
  Bool ->
  -- | Timestamp in milliseconds
  Word64 ->
  HealthStatus
unhealthyStatus agentId reason recoverable timestamp =
  HealthStatus
    { hsAgentId = agentId
    , hsState = StateUnhealthy reason recoverable
    , hsMessage = Nothing
    , hsLoad = Nothing
    , hsResources = Nothing
    , hsValidUntilMs = Nothing
    , hsTimestampMs = timestamp
    }

-- | Empty load metrics (all zeros)
emptyLoadMetrics :: LoadMetrics
emptyLoadMetrics =
  LoadMetrics
    { loadInFlight = 0
    , loadQueueDepth = 0
    , loadAvgLatencyMs = 0.0
    , loadP50LatencyMs = 0.0
    , loadP95LatencyMs = 0.0
    , loadP99LatencyMs = 0.0
    , loadRequestsProcessed = 0
    , loadRequestsRejected = 0
    , loadRequestsTimedOut = 0
    }

-- | Create load metrics with current values
newLoadMetrics ::
  -- | In-flight requests
  Word32 ->
  -- | Queue depth
  Word32 ->
  -- | Average latency (ms)
  Float ->
  -- | Requests processed
  Word64 ->
  LoadMetrics
newLoadMetrics inFlight queueDepth avgLatency processed =
  emptyLoadMetrics
    { loadInFlight = inFlight
    , loadQueueDepth = queueDepth
    , loadAvgLatencyMs = avgLatency
    , loadRequestsProcessed = processed
    }

-- | Empty resource metrics (all Nothing)
emptyResourceMetrics :: ResourceMetrics
emptyResourceMetrics =
  ResourceMetrics
    { resCpuPercent = Nothing
    , resMemoryBytes = Nothing
    , resMemoryLimit = Nothing
    , resActiveThreads = Nothing
    , resOpenFds = Nothing
    , resFdLimit = Nothing
    , resConnections = Nothing
    }

-- | Get current time in milliseconds since Unix epoch
getCurrentTimeMs :: MonadIO m => m Word64
getCurrentTimeMs = liftIO $ do
  t <- getPOSIXTime
  pure $ round (t * 1000)
