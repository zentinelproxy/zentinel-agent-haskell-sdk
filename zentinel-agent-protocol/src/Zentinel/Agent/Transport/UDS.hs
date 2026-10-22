-- |
-- Module      : Zentinel.Agent.Transport.UDS
-- Description : Agent protocol v2 over a Unix domain socket
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : experimental
--
-- The proxy opens one or more connections to the socket. On each it sends a
-- handshake request (0x01), which is answered with the agent's capabilities
-- (0x02); then a Configure message (0x17) carrying the agent's @config { }@
-- block, which is answered like any event before the proxy uses the
-- connection; then events. Requests are multiplexed over a connection, so
-- events are handled concurrently and every 'AgentResponse' (0x20) carries
-- its request's correlation id in @audit.custom.correlation_id@. Pings
-- (0x41) are answered with a Pong (0x42) carrying the same payload.
--
-- See "Zentinel.Agent.Wire" for the framing.
module Zentinel.Agent.Transport.UDS
  ( -- * Server
    UdsServerConfig (..)
  , defaultUdsConfig
  , runUdsServer

    -- * Dispatch (shared with the gRPC transport)
  , dispatchEvent
  , configure
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, catch, finally, try)
import Control.Monad (forever, void, when)
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecodeStrict, encode, fromJSON)
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.Socket
import System.Directory (removeFile)
import System.Posix.Files (setFileMode)
import Zentinel.Agent.Handler
import Zentinel.Agent.Internal.Logging
import Zentinel.Agent.Types
import Zentinel.Agent.Wire

-- | UDS server configuration
data UdsServerConfig = UdsServerConfig
  { udsSocketPath :: !FilePath
  -- ^ Path to the Unix socket
  , udsSocketMode :: !Word
  -- ^ Socket file permissions (default: 0o660)
  , udsMaxConnections :: !Int
  -- ^ Maximum concurrent connections
  , udsBacklog :: !Int
  -- ^ Listen backlog
  , udsLogAction :: !(LogAction IO LogMessage)
  -- ^ Logger
  }

-- | Default UDS configuration
defaultUdsConfig :: UdsServerConfig
defaultUdsConfig =
  UdsServerConfig
    { udsSocketPath = "/tmp/zentinel-agent.sock"
    , udsSocketMode = 0o660
    , udsMaxConnections = 100
    , udsBacklog = 128
    , udsLogAction = stderrLogger Info
    }

-- | Serve the agent on a Unix socket until the thread is killed.
runUdsServer ::
  forall m.
  AgentHandler m =>
  UdsServerConfig ->
  (forall a. m a -> IO a) ->
  IO ()
runUdsServer config runM = do
  let path = udsSocketPath config
      logger = udsLogAction config
  removeFile path `catch` \(_ :: SomeException) -> pure ()
  bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
    bind sock (SockAddrUnix path)
    setFileMode path (fromIntegral (udsSocketMode config))
    listen sock (udsBacklog config)
    logInfo logger $ "UDS server listening on " <> T.pack path
    active <- newTVarIO (0 :: Int)
    let acceptLoop = forever $ do
          admitted <- atomically $ do
            n <- readTVar active
            if n < udsMaxConnections config
              then writeTVar active (n + 1) >> pure True
              else pure False
          if admitted
            then do
              (client, _) <- accept sock
              void . forkIO $
                handleConnection logger runM client
                  `finally` (close client >> atomically (modifyTVar' active (subtract 1)))
            else threadDelay 10000
    acceptLoop `finally` (removeFile path `catch` \(_ :: SomeException) -> pure ())

-- | Serve one proxy connection: handshake, then messages until it closes.
handleConnection ::
  forall m.
  AgentHandler m =>
  LogAction IO LogMessage ->
  (forall a. m a -> IO a) ->
  Socket ->
  IO ()
handleConnection logger runM sock = do
  writeLock <- newMVar ()
  let send t payload =
        withMVar writeLock $ \_ ->
          writeFrame sock t payload >>= \case
            Left err -> logError logger $ "Could not send a frame: " <> T.pack (show err)
            Right () -> pure ()
      sendJSON :: ToJSON v => MessageType -> v -> IO ()
      sendJSON t v = send t (LBS.toStrict (encode v))
      loop = do
        frame <- readFrame sock
        case frame of
          Left err -> logError logger $ "Framing error: " <> T.pack (show err)
          Right Nothing -> logDebug logger "Connection closed by the proxy"
          Right (Just (byte, payload)) -> do
            case messageTypeFromByte byte of
              Just MsgPing -> send MsgPong payload
              Just MsgConfigure ->
                -- Before reading on: no request on this connection may be
                -- handled before its configuration is.
                respond MsgConfigure payload >>= sendJSON MsgAgentResponse
              Just t
                | Just _ <- eventMessageType t ->
                    void . forkIO $ respond t payload >>= sendJSON MsgAgentResponse
              Just MsgCancel -> pure ()
              _ -> logDebug logger $ "Ignoring message type " <> T.pack (show byte)
            loop
      respond t payload = do
        let value = either (const Null) id (eitherDecodeStrict payload :: Either String Value)
        resp <-
          try (dispatchEvent runM t payload) >>= \case
            Right r -> pure r
            Left (e :: SomeException) -> do
              logError logger $ "Handler failed: " <> T.pack (show e)
              pure allow
        pure (withCorrelationId (correlationIdOf value) resp)

  frame <- readFrame sock
  case frame of
    Right (Just (byte, payload))
      | messageTypeFromByte byte == Just MsgHandshakeRequest ->
          case eitherDecodeStrict payload of
            Left err -> logError logger $ "Invalid handshake request: " <> T.pack err
            Right (req :: HandshakeRequest) -> do
              caps <- runM capabilities
              let reply = udsHandshakeResponse caps (hsReqSupportedVersions req)
              sendJSON MsgHandshakeResponse reply
              when (protocolVersion `elem` hsReqSupportedVersions req) $ do
                logInfo logger $
                  "Handshake with " <> hsReqProxyId req <> " " <> hsReqProxyVersion req
                loop
    Right (Just (byte, _)) ->
      logError logger $ "Expected a handshake request, got message type " <> T.pack (show byte)
    Right Nothing -> pure ()
    Left err -> logError logger $ "Framing error: " <> T.pack (show err)

-- | Apply a configuration: allow if 'onConfigure' accepts it, a 500 block
-- (which the proxy treats as a rejection) otherwise.
configure :: AgentHandler m => Value -> Maybe Text -> m AgentResponse
configure config version = do
  accepted <- onConfigure config version
  pure $ if accepted then allow else block 500 "Configuration rejected"

-- | Handle one event message (JSON payload of the given type) and produce
-- the response, without the correlation id.
dispatchEvent ::
  forall m.
  AgentHandler m =>
  (forall a. m a -> IO a) ->
  MessageType ->
  ByteString ->
  IO AgentResponse
dispatchEvent runM t payload = case t of
  MsgConfigure -> do
    v <- decodeOr Null
    let (cfg, version) = configureFromPayload v
    runM (configure cfg version)
  MsgRequestHeaders -> withEvent onRequestHeaders
  MsgRequestBodyChunk -> withEvent onRequestBodyChunk
  MsgResponseHeaders -> withEvent onResponseHeaders
  MsgResponseBodyChunk -> withEvent onResponseBodyChunk
  MsgWebSocketFrame -> withEvent onWebSocketFrame
  MsgRequestComplete -> withEvent (\e -> onRequestComplete e >> pure allow)
  MsgGuardrailInspect -> withEvent $ \e -> do
    result <- onGuardrailInspect e
    -- The proxy reads a guardrail verdict from this routing metadata key.
    let raw = TE.decodeUtf8 (LBS.toStrict (encode result))
    pure allow {respRoutingMetadata = HM.singleton "guardrail_response" raw}
  _ -> pure allow
  where
    decodeOr :: Value -> IO Value
    decodeOr d = pure (either (const d) id (eitherDecodeStrict payload))
    withEvent :: FromJSON e => (e -> m AgentResponse) -> IO AgentResponse
    withEvent f = do
      v <- decodeOr Null
      case fromJSON v of
        A.Success e -> runM (f e)
        A.Error err -> fail ("could not decode " <> show t <> ": " <> err)
