# zentinel-agent-haskell

Haskell SDK for the [Zentinel Agent Protocol v2](https://zentinelproxy.io/docs/agents/v2/).

[![Hackage](https://img.shields.io/hackage/v/zentinel-agent-protocol.svg)](https://hackage.haskell.org/package/zentinel-agent-protocol)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)

## Overview

This SDK enables writing Zentinel agents in Haskell with full protocol v2 support. Agents are external processes that extend Zentinel's functionality by inspecting and modifying HTTP requests/responses.

### Features

- **Full Protocol v2 Support** - All event types, decisions, and control messages
- **Type-Safe** - Strongly typed API with compile-time guarantees
- **Flexible Effect System** - Uses `ReaderT env IO` with `unliftio` for safe async operations
- **Dual Transport** - Both UDS (Unix Domain Sockets) and gRPC
- **Health Reporting** - Built-in health status reporting
- **Metrics Export** - Prometheus-compatible metrics collection

## Quick Start

### Installation

Add to your `cabal` file:

```cabal
build-depends:
    zentinel-agent-protocol >= 0.1
```

Or with Stack:

```yaml
extra-deps:
  - zentinel-agent-protocol-0.1.0.0
```

### Minimal Agent

```haskell
module Main where

import Zentinel.Agent.Protocol

-- Simple agent that allows all requests
instance AgentHandler IO where
  capabilities = pure $ defaultCapabilities "my-agent"
  onRequestHeaders _ = pure allow

main :: IO ()
main = runAgent defaultConfig id
```

### Agent with State

For agents that need to maintain state, use the `ReaderT` pattern:

```haskell
module Main where

import Zentinel.Agent.Protocol
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.IORef
import Data.Text qualified as T

data WafAgent = WafAgent
  { blockedCount :: IORef Int
  , allowedCount :: IORef Int
  }

newWafAgent :: IO WafAgent
newWafAgent = WafAgent <$> newIORef 0 <*> newIORef 0

instance AgentHandler (ReaderT WafAgent IO) where
  capabilities = pure AgentCapabilities
    { capProtocolVersion = protocolVersion
    , capAgentId = "waf-agent-001"
    , capName = "waf-agent"
    , capVersion = "0.1.0"
    , capSupportedEvents = [RequestHeaders]
    , capFeatures = defaultFeatures { featHealthReporting = True }
    , capLimits = defaultLimits
    , capHealthConfig = defaultHealthConfig
    }

  onRequestHeaders event = do
    agent <- ask
    if "/admin" `T.isPrefixOf` reqHdrUri event
      then do
        liftIO $ atomicModifyIORef' (blockedCount agent) (\n -> (n + 1, ()))
        pure $ block 403 "Forbidden"
      else do
        liftIO $ atomicModifyIORef' (allowedCount agent) (\n -> (n + 1, ()))
        pure allow

  healthStatus = do
    timestamp <- getCurrentTimeMs
    pure $ healthyStatus "waf-agent-001" timestamp

main :: IO ()
main = do
  agent <- newWafAgent
  let config = defaultConfig { scSocketPath = Just "/tmp/waf-agent.sock" }
  runAgent config (`runReaderT` agent)
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Your Agent Application                    │
│  ┌─────────────────────────────────────────────────────────┐│
│  │              AgentHandler Implementation                ││
│  │  (capabilities, onRequestHeaders, healthStatus, etc.)   ││
│  └─────────────────────────────────────────────────────────┘│
│                            │                                 │
│  ┌─────────────────────────┴───────────────────────────────┐│
│  │              Zentinel Agent Protocol SDK                 ││
│  │  ┌───────────────┐ ┌───────────────┐ ┌───────────────┐  ││
│  │  │    Types      │ │    Server     │ │   Transport   │  ││
│  │  │   Handler     │ │    Health     │ │   UDS/gRPC    │  ││
│  │  └───────────────┘ └───────────────┘ └───────────────┘  ││
│  └─────────────────────────────────────────────────────────┘│
└──────────────────────────────┬──────────────────────────────┘
                               │
                    Unix Socket or gRPC
                               │
                               ▼
                    ┌─────────────────────┐
                    │   Zentinel Proxy    │
                    └─────────────────────┘
```

## Handler Methods

The `AgentHandler` typeclass defines the interface for agent implementations:

| Method | Required | Description |
|--------|----------|-------------|
| `capabilities` | Yes | Report agent capabilities at startup |
| `onRequestHeaders` | Yes | Handle incoming request headers |
| `onRequestBodyChunk` | No | Handle request body chunks (streaming) |
| `onResponseHeaders` | No | Handle response headers |
| `onResponseBodyChunk` | No | Handle response body chunks |
| `onRequestComplete` | No | Handle request completion (for logging) |
| `onWebSocketFrame` | No | Handle WebSocket frames |
| `onGuardrailInspect` | No | Handle guardrail inspection |
| `healthStatus` | No | Report health status |
| `metricsReport` | No | Report metrics |
| `onConfigure` | No | Handle configuration updates |
| `onShutdown` | No | Handle shutdown request |
| `onDrain` | No | Handle drain request |
| `onCancel` | No | Handle request cancellation |

## Decisions

Agents return decisions that control request/response flow:

```haskell
-- Allow the request to continue
allow :: AgentResponse

-- Block with HTTP status and message
block :: Word16 -> Text -> AgentResponse
block 403 "Access denied"

-- Redirect to another URL
redirect :: Text -> Word16 -> AgentResponse
redirect "https://example.com/login" 302

-- Issue a challenge (e.g., CAPTCHA)
challenge :: Text -> HashMap Text Text -> AgentResponse
challenge "captcha" (HM.singleton "site_key" "abc123")
```

## Configuration

### Server Configuration

```haskell
data ServerConfig = ServerConfig
  { scSocketPath  :: Maybe FilePath    -- UDS path
  , scGrpcAddress :: Maybe HostPort    -- gRPC address
  , scLogLevel    :: LogLevel          -- Minimum log level
  }

-- Default: UDS on /tmp/zentinel-agent.sock
defaultConfig :: ServerConfig

-- Custom configuration
let config = ServerConfig
      { scSocketPath = Just "/var/run/my-agent.sock"
      , scGrpcAddress = Just (HostPort "0.0.0.0" 50051)
      , scLogLevel = Info
      }
```

### Agent Capabilities

```haskell
AgentCapabilities
  { capProtocolVersion = 2
  , capAgentId = "my-agent-001"
  , capName = "my-agent"
  , capVersion = "1.0.0"
  , capSupportedEvents = [RequestHeaders, RequestBodyChunk]
  , capFeatures = AgentFeatures
      { featStreamingBody = True      -- Enable body streaming
      , featWebsocket = False         -- WebSocket inspection
      , featGuardrails = False        -- Guardrail inspection
      , featConfigPush = True         -- Support config updates
      , featMetricsExport = True      -- Report metrics
      , featConcurrentRequests = 100  -- Concurrent capacity
      , featCancellation = True       -- Support cancellation
      , featFlowControl = False       -- Flow control
      , featHealthReporting = True    -- Report health
      }
  , capLimits = AgentLimits
      { limMaxBodySize = 10 * 1024 * 1024       -- 10MB
      , limMaxConcurrency = 100
      , limPreferredChunkSize = 64 * 1024       -- 64KB
      , limMaxMemory = Just (512 * 1024 * 1024) -- 512MB
      , limMaxProcessingTimeMs = Just 5000      -- 5s
      }
  , capHealthConfig = HealthConfig
      { healthReportIntervalMs = 10000  -- 10s
      , healthIncludeLoad = True
      , healthIncludeResources = False
      }
  }
```

## Health Reporting

```haskell
healthStatus = do
  timestamp <- getCurrentTimeMs
  inFlight <- getInFlightCount

  if inFlight > threshold
    then pure $ degradedStatus agentId ["streaming"] 2.0 timestamp
    else pure $ healthyStatus agentId timestamp
```

Health states:

- `StateHealthy` - Fully operational
- `StateDegraded` - Operational but some features disabled
- `StateDraining` - Preparing for shutdown
- `StateUnhealthy` - Cannot process requests

## Metrics

```haskell
metricsReport = do
  timestamp <- getCurrentTimeMs
  processed <- getProcessedCount
  blocked <- getBlockedCount

  pure $ Just MetricsReport
    { mrAgentId = "my-agent-001"
    , mrTimestampMs = timestamp
    , mrIntervalMs = 10000
    , mrCounters =
        [ counterMetric metricRequestsTotal processed
        , counterMetric metricRequestsBlockedTotal blocked
        ]
    , mrGauges = []
    , mrHistograms = []
    }
```

Standard metric names:

- `agent_requests_total`
- `agent_requests_blocked_total`
- `agent_requests_duration_seconds`
- `agent_errors_total`
- `agent_in_flight_requests`
- `agent_queue_depth`

## Examples

See the `examples/` directory:

- `echo-agent/` - Minimal agent that allows all requests
- `waf-agent/` - WAF-style agent with blocking rules and metrics

## Requirements

- GHC 9.8+ (GHC2021 language standard)
- Zentinel proxy with agent support enabled

## Testing

```bash
# Build and run tests
cabal build all
cabal test all

# Run echo agent
cabal run echo-agent -- --socket /tmp/echo.sock

# Test with netcat (manual)
echo '{"type":"handshake","payload":{"supported_versions":[2],"proxy_id":"test","proxy_version":"1.0"}}' | nc -U /tmp/echo.sock
```

## License

Apache-2.0

## Contributing

Contributions welcome! Please see the [Zentinel contributing guide](https://zentinelproxy.io/docs/contributing).
