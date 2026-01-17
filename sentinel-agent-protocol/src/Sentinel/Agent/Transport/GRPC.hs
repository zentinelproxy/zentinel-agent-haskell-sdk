-- |
-- Module      : Sentinel.Agent.Transport.GRPC
-- Description : gRPC transport for Sentinel agents
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : experimental
--
-- This module implements the gRPC transport for Sentinel agents.
-- It provides a bidirectional streaming interface for communication
-- with the Sentinel proxy.
--
-- = Protocol
--
-- The gRPC transport uses bidirectional streaming:
--
-- * Proxy streams 'ProxyMessage' to the agent
-- * Agent streams 'AgentMessage' back
--
-- = Usage
--
-- @
-- runGrpcServer config handler
-- @
--
-- = Note
--
-- This module requires the @grpc-haskell@ package and protobuf definitions.
-- For production use, you should generate types from the .proto files
-- using @proto3-suite@.
module Sentinel.Agent.Transport.GRPC
  ( -- * Server
    GrpcServerConfig (..)
  , defaultGrpcConfig
  , runGrpcServer

    -- * Address
  , HostPort (..)

    -- * Errors
  , GrpcError (..)
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (Async, async, cancel, wait)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, bracket, catch, finally, throwIO)
import Control.Monad (forever, void, when)
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecodeStrict, encode, toJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word16, Word64)
import Sentinel.Agent.Handler
import Sentinel.Agent.Internal.Logging
import Sentinel.Agent.Types
import UnliftIO (MonadUnliftIO, liftIO)

-- | Host and port for gRPC server
data HostPort = HostPort
  { hpHost :: !Text
  , hpPort :: !Word16
  }
  deriving stock (Eq, Show)

-- | gRPC server configuration
data GrpcServerConfig = GrpcServerConfig
  { grpcAddress :: !HostPort
  -- ^ Address to bind to
  , grpcMaxMessageSize :: !Int
  -- ^ Maximum message size (default: 10MB)
  , grpcMaxConnections :: !Int
  -- ^ Maximum concurrent connections
  , grpcLogAction :: !(LogAction IO LogMessage)
  -- ^ Logger
  }

-- | Default gRPC configuration
defaultGrpcConfig :: GrpcServerConfig
defaultGrpcConfig =
  GrpcServerConfig
    { grpcAddress = HostPort "0.0.0.0" 50051
    , grpcMaxMessageSize = 10 * 1024 * 1024
    , grpcMaxConnections = 100
    , grpcLogAction = stderrLogger Info
    }

-- | gRPC transport errors
data GrpcError
  = GrpcBindError !String
  | GrpcHandshakeError !String
  | GrpcProtocolError !String
  | GrpcConnectionClosed
  | GrpcParseError !String
  deriving stock (Show, Eq)

instance Exception GrpcError

-- | Run the gRPC server
--
-- This is a placeholder implementation. A full implementation would use
-- the grpc-haskell package with generated protobuf types.
--
-- For now, this provides the interface that matches the SDK design,
-- allowing users to prepare their agent logic while the full gRPC
-- implementation is completed.
runGrpcServer ::
  forall m.
  (MonadUnliftIO m, AgentHandler m) =>
  GrpcServerConfig ->
  (forall a. m a -> IO a) ->
  IO ()
runGrpcServer config runM = do
  -- Get capabilities first
  caps <- runM capabilities

  let logger = grpcLogAction config
      addr = grpcAddress config

  logInfo logger $
    "gRPC server starting on "
      <> hpHost addr
      <> ":"
      <> T.pack (show (hpPort addr))

  -- NOTE: Full gRPC implementation would go here
  -- This requires:
  -- 1. Proto file definitions matching the Rust implementation
  -- 2. Generated Haskell types from proto3-suite
  -- 3. grpc-haskell server setup
  --
  -- For now, we log a message and block
  logWarn logger "gRPC transport is not yet fully implemented"
  logInfo logger "Use UDS transport for now, or contribute the gRPC implementation"

  -- Block forever (or until shutdown)
  forever $ threadDelay maxBound

-- | Serialize a message for gRPC (JSON encoding)
--
-- In a full implementation, this would use protobuf encoding.
serializeMessage :: ToJSON a => a -> ByteString
serializeMessage = LBS.toStrict . encode

-- | Deserialize a message from gRPC
--
-- In a full implementation, this would use protobuf decoding.
deserializeMessage :: FromJSON a => ByteString -> Either String a
deserializeMessage = eitherDecodeStrict


{-
-- FULL GRPC IMPLEMENTATION SKETCH
--
-- When implementing the full gRPC transport, use this structure:
--
-- 1. Define the service in a .proto file:
--
-- service AgentService {
--   rpc Process(stream ProxyMessage) returns (stream AgentMessage);
-- }
--
-- 2. Generate Haskell types:
--
-- $ compile-proto-file --proto sentinel_agent.proto --out src
--
-- 3. Implement the service handler:
--
-- processHandler :: AgentHandler m => ServerBiDiStreaming ProxyMessage AgentMessage
-- processHandler = ServerBiDiStreaming $ \recv send -> do
--   -- Perform handshake
--   msg <- recv
--   case msg of
--     Just (ProxyMsgHandshake req) -> do
--       caps <- capabilities
--       send (AgentMsgHandshakeResponse (makeHandshakeResponse caps))
--       -- Enter main loop
--       processLoop recv send
--     _ -> throwIO (GrpcHandshakeError "Expected handshake")
--
-- processLoop recv send = do
--   msg <- recv
--   case msg of
--     Nothing -> pure ()  -- Stream ended
--     Just proxyMsg -> do
--       response <- handleProxyMessage proxyMsg
--       mapM_ send response
--       processLoop recv send
--
-- 4. Register and run the server:
--
-- runGrpcServerFull config runM = do
--   let handlers = [unary (RPC :: RPC AgentService "process") processHandler]
--   runServer (ServerConfig ...) handlers
-}
