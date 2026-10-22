{-# LANGUAGE CPP #-}

-- |
-- Module      : Zentinel.Agent.Server
-- Description : Server runner for Zentinel agents
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : experimental
--
-- Starts the configured transports (a Unix socket and/or gRPC) and serves
-- the agent until the process is stopped.
--
-- @
-- main :: IO ()
-- main = do
--   config <- configFromArgs defaultConfig   -- --socket PATH, --grpc HOST:PORT
--   runAgent config id
-- @
--
-- For an agent with state:
--
-- @
-- main :: IO ()
-- main = do
--   env <- newEnv
--   config <- configFromArgs defaultConfig
--   runAgent config (flip runReaderT env)
-- @
module Zentinel.Agent.Server
  ( -- * Configuration
    ServerConfig (..)
  , defaultConfig
  , configFromArgs
  , parseArgs
  , HostPort (..)
  , parseHostPort
  , grpcAvailable

    -- * Server Runner
  , runAgent
  , runAgentWith

    -- * Logging
  , LogLevel (..)
  ) where

import Control.Concurrent.Async (async, waitAnyCancel)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Zentinel.Agent.Handler
import Zentinel.Agent.Internal.Logging (stderrLogger)
import Zentinel.Agent.Internal.Logging qualified as L
import Zentinel.Agent.Transport.UDS (UdsServerConfig (..), runUdsServer)
import Zentinel.Agent.Types (AgentCapabilities (..))
#ifdef ZENTINEL_GRPC
import Zentinel.Agent.Transport.GRPC (GrpcServerConfig (..), runGrpcServer)
#endif

-- | Log level for the server
data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving stock (Eq, Ord, Show, Bounded, Enum)

-- | A TCP address for the gRPC transport.
data HostPort = HostPort
  { hpHost :: !Text
  , hpPort :: !Word16
  }
  deriving stock (Eq, Show)

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

-- | Default server configuration: the Unix socket @/tmp/zentinel-agent.sock@.
defaultConfig :: ServerConfig
defaultConfig =
  ServerConfig
    { scSocketPath = Just "/tmp/zentinel-agent.sock"
    , scGrpcAddress = Nothing
    , scLogLevel = Info
    }

-- | Whether this build of the SDK includes the gRPC transport (the @grpc@
-- cabal flag, on by default).
grpcAvailable :: Bool
#ifdef ZENTINEL_GRPC
grpcAvailable = True
#else
grpcAvailable = False
#endif

-- | Apply the command line: @--socket PATH@, @--grpc HOST:PORT@, @--debug@.
-- Giving only @--grpc@ serves gRPC alone; giving both serves both.
configFromArgs :: ServerConfig -> IO ServerConfig
configFromArgs cfg = parseArgs cfg <$> getArgs

-- | 'configFromArgs' on a given argument list.
parseArgs :: ServerConfig -> [String] -> ServerConfig
parseArgs cfg0 args = go (if "--grpc" `elem` args && "--socket" `notElem` args then cfg0 {scSocketPath = Nothing} else cfg0) args
  where
    go cfg [] = cfg
    go cfg ("--socket" : path : rest) = go cfg {scSocketPath = Just path} rest
    go cfg ("--grpc" : hostPort : rest) = go cfg {scGrpcAddress = parseHostPort hostPort} rest
    go cfg ("--debug" : rest) = go cfg {scLogLevel = Debug} rest
    go cfg (_ : rest) = go cfg rest

-- | Parse @host:port@.
parseHostPort :: String -> Maybe HostPort
parseHostPort s =
  case break (== ':') (reverse s) of
    (revPort, ':' : revHost) | [(port, "")] <- reads (reverse revPort) ->
      Just (HostPort (T.pack (reverse revHost)) port)
    _ -> Nothing

-- | Run the agent until the process is stopped.
--
-- @
-- instance AgentHandler IO where
--   capabilities = pure (defaultCapabilities "echo-agent")
--   onRequestHeaders _ = pure allow
--
-- main :: IO ()
-- main = configFromArgs defaultConfig >>= \\cfg -> runAgent cfg id
-- @
runAgent ::
  forall m.
  AgentHandler m =>
  ServerConfig ->
  (forall a. m a -> IO a) ->
  IO ()
runAgent config runM = runAgentWith config runM (pure ())

-- | 'runAgent' with a setup action, run in the agent's monad first.
runAgentWith ::
  forall m.
  AgentHandler m =>
  ServerConfig ->
  (forall a. m a -> IO a) ->
  m () ->
  IO ()
runAgentWith config runM setup = do
  let logger = stderrLogger (toSeverity (scLogLevel config))
  runM setup
  caps <- runM capabilities
  L.logInfo logger $ "Starting agent: " <> capName caps <> " v" <> capVersion caps

  udsServer <- case scSocketPath config of
    Nothing -> pure []
    Just path ->
      fmap pure . async $
        runUdsServer
          UdsServerConfig
            { udsSocketPath = path
            , udsSocketMode = 0o660
            , udsMaxConnections = 100
            , udsBacklog = 128
            , udsLogAction = logger
            }
          runM

  grpcServer <- case scGrpcAddress config of
    Nothing -> pure []
#ifdef ZENTINEL_GRPC
    Just addr ->
      fmap pure . async $
        runGrpcServer
          GrpcServerConfig
            { grpcHost = hpHost addr
            , grpcPort = hpPort addr
            , grpcLogAction = logger
            }
          runM
#else
    Just _ -> do
      L.logError logger "This build has no gRPC transport (cabal flag grpc is off)"
      exitFailure
#endif

  case udsServer <> grpcServer of
    [] -> do
      L.logError logger "No transport configured: give a socket path or a gRPC address"
      exitFailure
    servers -> do
      _ <- waitAnyCancel servers
      L.logInfo logger "Agent shutting down"

toSeverity :: LogLevel -> L.Severity
toSeverity = \case
  Debug -> L.Debug
  Info -> L.Info
  Warn -> L.Warn
  Error -> L.Error
