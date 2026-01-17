-- |
-- Module      : Sentinel.Agent.Server
-- Description : Server runner for Sentinel agents
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : experimental
--
-- This module provides the main server runner for Sentinel agents.
-- It handles starting the appropriate transports (UDS and/or gRPC)
-- and managing the agent lifecycle.
--
-- = Quick Start
--
-- For a simple agent running on UDS:
--
-- @
-- main :: IO ()
-- main = runAgent defaultConfig id
-- @
--
-- For an agent with state:
--
-- @
-- main :: IO ()
-- main = do
--   agent <- newMyAgent
--   runAgent config (flip runReaderT agent)
-- @
--
-- = Multiple Transports
--
-- You can run both UDS and gRPC simultaneously:
--
-- @
-- let config = defaultConfig
--       { scSocketPath = Just "/tmp/my-agent.sock"
--       , scGrpcAddress = Just (HostPort "0.0.0.0" 50051)
--       }
-- runAgent config handler
-- @
module Sentinel.Agent.Server
  ( -- * Configuration
    ServerConfig (..)
  , defaultConfig

    -- * Server Runner
  , runAgent
  , runAgentWith

    -- * Logging
  , LogLevel (..)

    -- * Re-exports
  , HostPort (..)
  ) where

import Control.Concurrent.Async (Async, async, cancel, waitAnyCancel)
import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Sentinel.Agent.Handler
import Sentinel.Agent.Internal.Logging
import Sentinel.Agent.Transport.GRPC (GrpcServerConfig (..), HostPort (..), runGrpcServer)
import Sentinel.Agent.Transport.UDS (UdsServerConfig (..), runUdsServer)
import Sentinel.Agent.Types
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import UnliftIO (MonadUnliftIO, liftIO)

-- | Log level for the server
data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving stock (Eq, Ord, Show, Bounded, Enum)

-- | Server configuration
data ServerConfig = ServerConfig
  { scSocketPath :: !(Maybe FilePath)
  -- ^ Unix socket path (Nothing to disable UDS)
  , scGrpcAddress :: !(Maybe HostPort)
  -- ^ gRPC address (Nothing to disable gRPC)
  , scLogLevel :: !LogLevel
  -- ^ Minimum log level
  }
  deriving stock (Eq, Show)

-- | Default server configuration
--
-- Uses UDS transport at @/tmp/sentinel-agent.sock@
defaultConfig :: ServerConfig
defaultConfig =
  ServerConfig
    { scSocketPath = Just "/tmp/sentinel-agent.sock"
    , scGrpcAddress = Nothing
    , scLogLevel = Info
    }

-- | Run the agent server
--
-- This is the main entry point for running an agent. It starts the
-- configured transports and blocks until shutdown.
--
-- = Example: Simple IO Agent
--
-- @
-- instance AgentHandler IO where
--   capabilities = pure $ defaultCapabilities "echo-agent"
--   onRequestHeaders _ = pure allow
--
-- main :: IO ()
-- main = runAgent defaultConfig id
-- @
--
-- = Example: Agent with ReaderT
--
-- @
-- data MyAgent = MyAgent { counter :: IORef Int }
--
-- instance AgentHandler (ReaderT MyAgent IO) where
--   capabilities = pure $ defaultCapabilities "my-agent"
--   onRequestHeaders _ = do
--     agent <- ask
--     atomicModifyIORef' agent.counter (\\n -> (n + 1, ()))
--     pure allow
--
-- main :: IO ()
-- main = do
--   agent <- MyAgent <$> newIORef 0
--   runAgent defaultConfig (flip runReaderT agent)
-- @
runAgent ::
  forall m.
  (MonadUnliftIO m, AgentHandler m) =>
  ServerConfig ->
  (forall a. m a -> IO a) ->
  IO ()
runAgent config runM = runAgentWith config runM (pure ())

-- | Run the agent server with a setup action
--
-- The setup action is run before starting the servers.
-- This is useful for initialization that needs to happen
-- in the agent's monad context.
--
-- @
-- runAgentWith config runM $ do
--   logInfo "Agent starting up..."
--   loadConfiguration
-- @
runAgentWith ::
  forall m.
  (MonadUnliftIO m, AgentHandler m) =>
  ServerConfig ->
  (forall a. m a -> IO a) ->
  m () ->
  IO ()
runAgentWith config runM setup = do
  let logger = toLogAction (scLogLevel config)

  -- Run setup
  runM setup

  -- Get capabilities for logging
  caps <- runM capabilities
  logInfo logger $ "Starting agent: " <> capName caps <> " v" <> capVersion caps

  -- Validate configuration
  when (scSocketPath config == Nothing && scGrpcAddress config == Nothing) $ do
    logError logger "No transport configured. Enable at least UDS or gRPC."
    exitFailure

  -- Start transports
  servers <- startServers config runM logger

  case servers of
    [] -> do
      logError logger "Failed to start any transport"
      exitFailure
    _ -> do
      logInfo logger $ "Running " <> T.pack (show (length servers)) <> " transport(s)"
      -- Wait for any server to exit (usually on shutdown)
      _ <- waitAnyCancel servers
      logInfo logger "Agent shutting down"

-- | Start all configured servers
startServers ::
  forall m.
  (MonadUnliftIO m, AgentHandler m) =>
  ServerConfig ->
  (forall a. m a -> IO a) ->
  LogAction IO LogMessage ->
  IO [Async ()]
startServers config runM logger = do
  udsServer <- startUds
  grpcServer <- startGrpc
  pure $ concat [udsServer, grpcServer]
  where
    startUds = case scSocketPath config of
      Nothing -> pure []
      Just path -> do
        let udsConfig =
              UdsServerConfig
                { udsSocketPath = path
                , udsSocketMode = 0o660
                , udsMaxConnections = 100
                , udsBacklog = 128
                , udsLogAction = logger
                }
        server <- async $ runUdsServer udsConfig runM
        pure [server]

    startGrpc = case scGrpcAddress config of
      Nothing -> pure []
      Just addr -> do
        let grpcConfig =
              GrpcServerConfig
                { grpcAddress = addr
                , grpcMaxMessageSize = 10 * 1024 * 1024
                , grpcMaxConnections = 100
                , grpcLogAction = logger
                }
        server <- async $ runGrpcServer grpcConfig runM
        pure [server]

-- | Convert LogLevel to LogAction
toLogAction :: LogLevel -> LogAction IO LogMessage
toLogAction level = stderrLogger (toInternalLevel level)
  where
    toInternalLevel Debug = Sentinel.Agent.Internal.Logging.Debug
    toInternalLevel Info = Sentinel.Agent.Internal.Logging.Info
    toInternalLevel Warn = Sentinel.Agent.Internal.Logging.Warn
    toInternalLevel Error = Sentinel.Agent.Internal.Logging.Error
