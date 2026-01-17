-- |
-- Module      : Main
-- Description : WAF-style agent example
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
--
-- A more complete example demonstrating:
--
-- * Stateful agent using ReaderT
-- * Request blocking based on path patterns
-- * Metrics collection
-- * Health reporting with load metrics
--
-- = Usage
--
-- @
-- cabal run waf-agent -- --socket /tmp/waf-agent.sock
-- @
module Main where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.HashMap.Strict qualified as HM
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64)
import Sentinel.Agent.Protocol
import System.Environment (getArgs)

-- | WAF agent state
data WafAgent = WafAgent
  { blockedCount :: !(IORef Word64)
  -- ^ Number of blocked requests
  , allowedCount :: !(IORef Word64)
  -- ^ Number of allowed requests
  , inFlightCount :: !(IORef Word32)
  -- ^ Current in-flight requests
  }

-- | Create a new WAF agent
newWafAgent :: IO WafAgent
newWafAgent =
  WafAgent
    <$> newIORef 0
    <*> newIORef 0
    <*> newIORef 0

-- | WAF agent handler
instance AgentHandler (ReaderT WafAgent IO) where
  capabilities =
    pure
      AgentCapabilities
        { capProtocolVersion = protocolVersion
        , capAgentId = "waf-agent-001"
        , capName = "waf-agent"
        , capVersion = "0.1.0"
        , capSupportedEvents = [RequestHeaders, RequestComplete]
        , capFeatures =
            defaultFeatures
              { featHealthReporting = True
              , featMetricsExport = True
              , featConcurrentRequests = 100
              }
        , capLimits = defaultLimits
        , capHealthConfig =
            defaultHealthConfig
              { healthIncludeLoad = True
              }
        }

  onRequestHeaders event = do
    agent <- ask
    -- Track in-flight
    liftIO $ atomicModifyIORef' (inFlightCount agent) (\n -> (n + 1, ()))

    let uri = reqHdrUri event
        method = reqHdrMethod event

    -- Check for blocked patterns
    if isBlocked uri
      then do
        liftIO $ atomicModifyIORef' (blockedCount agent) (\n -> (n + 1, ()))
        pure $
          block 403 "Access denied by WAF policy"
            `withAudit` AuditMetadata
              { auditTags = ["waf", "blocked"]
              , auditRuleIds = [matchedRule uri]
              , auditConfidence = Just 1.0
              , auditReasonCodes = ["path_blocked"]
              , auditCustom = HM.empty
              }
      else do
        liftIO $ atomicModifyIORef' (allowedCount agent) (\n -> (n + 1, ()))
        pure allow

  onRequestComplete _event = do
    agent <- ask
    -- Track in-flight completion
    liftIO $ atomicModifyIORef' (inFlightCount agent) (\n -> (max 0 (n - 1), ()))

  healthStatus = do
    agent <- ask
    timestamp <- getCurrentTimeMs
    inFlight <- liftIO $ readIORef (inFlightCount agent)
    processed <- liftIO $ readIORef (allowedCount agent)
    blocked <- liftIO $ readIORef (blockedCount agent)

    let total = processed + blocked
        loadMetrics =
          LoadMetrics
            { loadInFlight = inFlight
            , loadQueueDepth = 0
            , loadAvgLatencyMs = 0.5  -- placeholder
            , loadP50LatencyMs = 0.3
            , loadP95LatencyMs = 1.0
            , loadP99LatencyMs = 2.0
            , loadRequestsProcessed = total
            , loadRequestsRejected = blocked
            , loadRequestsTimedOut = 0
            }

    -- Report degraded if too many in-flight
    if inFlight > 80
      then
        pure $
          (degradedStatus "waf-agent-001" ["streaming_body"] 1.5 timestamp)
            { hsLoad = Just loadMetrics
            }
      else
        pure $
          (healthyStatus "waf-agent-001" timestamp)
            { hsLoad = Just loadMetrics
            }

  metricsReport = do
    agent <- ask
    timestamp <- getCurrentTimeMs
    blocked <- liftIO $ readIORef (blockedCount agent)
    allowed <- liftIO $ readIORef (allowedCount agent)
    inFlight <- liftIO $ readIORef (inFlightCount agent)

    pure $
      Just
        MetricsReport
          { mrAgentId = "waf-agent-001"
          , mrTimestampMs = timestamp
          , mrIntervalMs = 10000
          , mrCounters =
              [ counterWithLabels
                  metricRequestsTotal
                  (HM.singleton "status" "allowed")
                  allowed
              , counterWithLabels
                  metricRequestsBlockedTotal
                  (HM.singleton "reason" "path_blocked")
                  blocked
              ]
          , mrGauges =
              [ gaugeMetric metricInFlightRequests (fromIntegral inFlight)
              ]
          , mrHistograms = []
          }

-- | Check if a URI should be blocked
isBlocked :: Text -> Bool
isBlocked uri =
  any (`T.isPrefixOf` uri) blockedPrefixes
    || any (`T.isInfixOf` uri) blockedPatterns
  where
    blockedPrefixes =
      [ "/admin"
      , "/debug"
      , "/.git"
      , "/.env"
      , "/wp-admin"
      , "/phpmyadmin"
      ]
    blockedPatterns =
      [ ".."
      , "etc/passwd"
      , "<script"
      , "SELECT * FROM"
      , "UNION SELECT"
      ]

-- | Get the matched rule for a blocked URI
matchedRule :: Text -> Text
matchedRule uri
  | "/admin" `T.isPrefixOf` uri = "block-admin"
  | "/debug" `T.isPrefixOf` uri = "block-debug"
  | "/.git" `T.isPrefixOf` uri = "block-git"
  | "/.env" `T.isPrefixOf` uri = "block-env"
  | ".." `T.isInfixOf` uri = "path-traversal"
  | "etc/passwd" `T.isInfixOf` uri = "lfi-attempt"
  | "<script" `T.isInfixOf` uri = "xss-attempt"
  | "SELECT" `T.isInfixOf` uri = "sqli-attempt"
  | otherwise = "generic-block"

-- | Add audit metadata to a response
withAudit :: AgentResponse -> AuditMetadata -> AgentResponse
withAudit resp audit = resp {respAudit = audit}

main :: IO ()
main = do
  args <- getArgs
  let config = parseArgs args defaultConfig
  agent <- newWafAgent
  putStrLn $ "Starting WAF agent on " <> show (scSocketPath config)
  runAgent config (`runReaderT` agent)

-- | Parse command line arguments
parseArgs :: [String] -> ServerConfig -> ServerConfig
parseArgs [] cfg = cfg
parseArgs ("--socket" : path : rest) cfg =
  parseArgs rest cfg {scSocketPath = Just path}
parseArgs ("--grpc" : hostPort : rest) cfg =
  case parseHostPort hostPort of
    Just hp -> parseArgs rest cfg {scGrpcAddress = Just hp}
    Nothing -> parseArgs rest cfg
parseArgs ("--debug" : rest) cfg =
  parseArgs rest cfg {scLogLevel = Debug}
parseArgs (_ : rest) cfg = parseArgs rest cfg

-- | Parse host:port string
parseHostPort :: String -> Maybe HostPort
parseHostPort s =
  case break (== ':') s of
    (host, ':' : portStr) ->
      case reads portStr of
        [(port, "")] -> Just (HostPort (T.pack host) port)
        _ -> Nothing
    _ -> Nothing
