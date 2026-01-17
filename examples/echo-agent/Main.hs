-- |
-- Module      : Main
-- Description : Simple echo agent example
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
--
-- A minimal Sentinel agent that allows all requests.
-- This demonstrates the basic structure of an agent.
--
-- = Usage
--
-- @
-- cabal run echo-agent -- --socket /tmp/echo-agent.sock
-- @
module Main where

import Data.Text qualified as T
import Sentinel.Agent.Protocol
import System.Environment (getArgs)

-- | Simple echo agent that allows all requests
--
-- This is a stateless agent that runs directly in IO.
-- For agents that need state, see the waf-agent example.
instance AgentHandler IO where
  capabilities =
    pure
      AgentCapabilities
        { capProtocolVersion = protocolVersion
        , capAgentId = "echo-agent-001"
        , capName = "echo-agent"
        , capVersion = "0.1.0"
        , capSupportedEvents = [RequestHeaders]
        , capFeatures = defaultFeatures {featHealthReporting = True}
        , capLimits = defaultLimits
        , capHealthConfig = defaultHealthConfig
        }

  onRequestHeaders event = do
    -- Log the request (in production, use proper structured logging)
    putStrLn $ "Request: " <> T.unpack (reqHdrMethod event) <> " " <> T.unpack (reqHdrUri event)
    -- Allow all requests
    pure allow

  healthStatus = do
    timestamp <- getCurrentTimeMs
    pure $ healthyStatus "echo-agent-001" timestamp

main :: IO ()
main = do
  args <- getArgs
  let config = parseArgs args defaultConfig
  putStrLn $ "Starting echo agent on " <> show (scSocketPath config)
  runAgent config id

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
