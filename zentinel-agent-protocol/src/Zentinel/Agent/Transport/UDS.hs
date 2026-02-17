-- |
-- Module      : Zentinel.Agent.Transport.UDS
-- Description : Unix Domain Socket transport for Zentinel agents
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : experimental
--
-- This module implements the UDS (Unix Domain Socket) transport for
-- Zentinel agents. It handles connection management, message framing,
-- and the handshake protocol.
--
-- = Wire Protocol
--
-- Messages are framed with a 4-byte big-endian length prefix:
--
-- @
-- +----------------+------------------+
-- | Length (4 bytes) | JSON Payload     |
-- | big-endian u32   | (length bytes)   |
-- +----------------+------------------+
-- @
--
-- = Usage
--
-- @
-- runUdsServer config handler
-- @
module Zentinel.Agent.Transport.UDS
  ( -- * Server
    UdsServerConfig (..)
  , defaultUdsConfig
  , runUdsServer

    -- * Connection
  , UdsConnection (..)

    -- * Errors
  , UdsError (..)
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, bracket, catch, finally, throwIO)
import Control.Monad (forever, void, when)
import Data.Aeson (ToJSON, Value (..), toJSON)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import Network.Socket hiding (recv, send)
import Zentinel.Agent.Handler
import Zentinel.Agent.Internal.Framing
import Zentinel.Agent.Internal.Logging
import Zentinel.Agent.Types
import System.Directory (removeFile)
import System.Posix.Files (setFileMode)
import UnliftIO (MonadUnliftIO, liftIO, withRunInIO)

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

-- | UDS transport errors
data UdsError
  = UdsBindError !String
  | UdsAcceptError !String
  | UdsHandshakeError !String
  | UdsProtocolError !String
  | UdsConnectionClosed
  | UdsFramingError !FramingError
  deriving stock (Show, Eq)

instance Exception UdsError

-- | Active UDS connection
data UdsConnection = UdsConnection
  { udsConnSocket :: !Socket
  , udsConnPeerId :: !Text
  , udsConnPeerVersion :: !Text
  }

-- | Run the UDS server
runUdsServer ::
  forall m.
  (MonadUnliftIO m, AgentHandler m) =>
  UdsServerConfig ->
  (forall a. m a -> IO a) ->
  IO ()
runUdsServer config runM = do
  -- Get capabilities first
  caps <- runM capabilities

  -- Remove existing socket file if present
  removeFile (udsSocketPath config) `catch` \(_ :: SomeException) -> pure ()

  -- Create and bind socket
  sock <- socket AF_UNIX Stream defaultProtocol
  bind sock (SockAddrUnix (udsSocketPath config))

  -- Set socket permissions
  setFileMode (udsSocketPath config) (fromIntegral (udsSocketMode config))

  -- Start listening
  listen sock (udsBacklog config)

  logInfo (udsLogAction config) $
    "UDS server listening on " <> T.pack (udsSocketPath config)

  -- Track active connections
  activeConns <- newTVarIO (0 :: Int)

  -- Accept loop
  let acceptLoop = forever $ do
        -- Check connection limit
        canAccept <- atomically $ do
          n <- readTVar activeConns
          if n < udsMaxConnections config
            then do
              writeTVar activeConns (n + 1)
              pure True
            else pure False

        if canAccept
          then do
            (clientSock, _) <- accept sock
            void $
              forkIO $
                handleConnection config runM caps clientSock
                  `finally` atomically (modifyTVar' activeConns (subtract 1))
          else do
            -- Wait before retrying
            threadDelay 10000

  acceptLoop `finally` do
    close sock
    removeFile (udsSocketPath config) `catch` \(_ :: SomeException) -> pure ()

-- | Handle a single connection
handleConnection ::
  forall m.
  (MonadUnliftIO m, AgentHandler m) =>
  UdsServerConfig ->
  (forall a. m a -> IO a) ->
  AgentCapabilities ->
  Socket ->
  IO ()
handleConnection config runM caps clientSock = do
  let logger = udsLogAction config

  -- Perform handshake
  handshakeResult <- performHandshake logger caps clientSock

  case handshakeResult of
    Left err -> do
      logError logger $ "Handshake failed: " <> T.pack (show err)
      close clientSock
    Right conn -> do
      logInfo logger $ "Connection established with " <> udsConnPeerId conn

      -- Message handling loop
      messageLoop config runM conn
        `catch` \(e :: SomeException) -> do
          logWarn logger $ "Connection error: " <> T.pack (show e)
        `finally` do
          logInfo logger $ "Connection closed: " <> udsConnPeerId conn
          close (udsConnSocket conn)

-- | Perform handshake with proxy
performHandshake ::
  LogAction IO LogMessage ->
  AgentCapabilities ->
  Socket ->
  IO (Either UdsError UdsConnection)
performHandshake logger caps sock = do
  -- Read handshake request
  msgResult <- readFramedMessage sock
  case msgResult of
    Left err -> pure $ Left (UdsFramingError err)
    Right (proxyMsg :: ProxyMessage) ->
      case proxyMsg of
        ProxyMsgHandshake req -> do
          logDebug logger $ "Received handshake from " <> hsReqProxyId req

          -- Negotiate protocol version
          let requestedVersions = hsReqSupportedVersions req
              ourVersion = capProtocolVersion caps
              negotiatedVersion =
                if ourVersion `elem` requestedVersions
                  then ourVersion
                  else
                    if 2 `elem` requestedVersions
                      then 2
                      else head requestedVersions

          -- Send handshake response
          let response =
                HandshakeResponse
                  { hsRespProtocolVersion = negotiatedVersion
                  , hsRespCapabilities = caps
                  , hsRespSuccess = True
                  , hsRespError = Nothing
                  }
              agentMsg = AgentMsgHandshakeResponse response

          writeFramedMessage sock agentMsg

          pure $
            Right
              UdsConnection
                { udsConnSocket = sock
                , udsConnPeerId = hsReqProxyId req
                , udsConnPeerVersion = hsReqProxyVersion req
                }
        _ ->
          pure $ Left (UdsHandshakeError "Expected handshake message")

-- | Main message handling loop
messageLoop ::
  forall m.
  (MonadUnliftIO m, AgentHandler m) =>
  UdsServerConfig ->
  (forall a. m a -> IO a) ->
  UdsConnection ->
  IO ()
messageLoop config runM conn = do
  let sock = udsConnSocket conn
      logger = udsLogAction config

  forever $ do
    -- Read next message
    msgResult <- readFramedMessage sock
    case msgResult of
      Left UnexpectedEof -> do
        logDebug logger "Connection closed by peer"
        throwIO UdsConnectionClosed
      Left err -> do
        logError logger $ "Framing error: " <> T.pack (show err)
        throwIO (UdsFramingError err)
      Right (proxyMsg :: ProxyMessage) -> do
        -- Handle message and get response
        responseMsg <- handleMessage runM proxyMsg
        case responseMsg of
          Just msg -> writeFramedMessage sock msg
          Nothing -> pure ()

-- | Handle a proxy message and return optional response
handleMessage ::
  forall m.
  (MonadUnliftIO m, AgentHandler m) =>
  (forall a. m a -> IO a) ->
  ProxyMessage ->
  IO (Maybe AgentMessage)
handleMessage runM = \case
  ProxyMsgRequestHeaders event -> do
    response <- runM (onRequestHeaders event)
    pure $ Just (AgentMsgResponse response)
  ProxyMsgRequestBodyChunk event -> do
    response <- runM (onRequestBodyChunk event)
    pure $ Just (AgentMsgResponse response)
  ProxyMsgResponseHeaders event -> do
    response <- runM (onResponseHeaders event)
    pure $ Just (AgentMsgResponse response)
  ProxyMsgResponseBodyChunk event -> do
    response <- runM (onResponseBodyChunk event)
    pure $ Just (AgentMsgResponse response)
  ProxyMsgRequestComplete event -> do
    runM (onRequestComplete event)
    pure Nothing
  ProxyMsgWebSocketFrame event -> do
    response <- runM (onWebSocketFrame event)
    pure $ Just (AgentMsgResponse response)
  ProxyMsgGuardrailInspect event -> do
    response <- runM (onGuardrailInspect event)
    pure $ Just (AgentMsgGuardrailResponse response)
  ProxyMsgCancel cancel -> do
    runM (onCancel cancel)
    pure Nothing
  ProxyMsgConfigUpdate req -> do
    accepted <- runM (onConfigure (toJSON (cfgUpdateType req)) (Just (cfgRequestId req)))
    timestamp <- getCurrentTimeMs
    let response =
          ConfigUpdateResponse
            { cfgRespRequestId = cfgRequestId req
            , cfgRespAccepted = accepted
            , cfgRespError = Nothing
            , cfgRespTimestampMs = timestamp
            }
    pure $ Just (AgentMsgConfigResponse response)
  ProxyMsgShutdown reason timeout -> do
    runM (onShutdown reason timeout)
    pure Nothing
  ProxyMsgDrain timeout reason -> do
    runM (onDrain timeout reason)
    pure Nothing
  ProxyMsgFlowControl _signal -> do
    -- Flow control signals are handled internally
    pure Nothing
  ProxyMsgHandshake _ -> do
    -- Handshake should have already been handled
    pure Nothing

-- | Get current time in milliseconds
getCurrentTimeMs :: IO Word64
getCurrentTimeMs = do
  t <- getPOSIXTime
  pure $ round (t * 1000)
