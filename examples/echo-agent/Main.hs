-- |
-- Module      : Main
-- Description : Simple echo agent example
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
--
-- A minimal Zentinel agent that allows all requests.
-- This demonstrates the basic structure of an agent.
--
-- = Usage
--
-- @
-- cabal run echo-agent -- --socket /tmp/echo-agent.sock
-- @
module Main where

import Data.Text qualified as T
import Zentinel.Agent.Protocol

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
  config <- configFromArgs defaultConfig
  putStrLn $ "Starting echo agent on " <> show (scSocketPath config)
  runAgent config id
