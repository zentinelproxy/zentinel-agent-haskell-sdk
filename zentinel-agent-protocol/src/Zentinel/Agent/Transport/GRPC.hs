-- |
-- Module      : Zentinel.Agent.Transport.GRPC
-- Description : gRPC transport for Zentinel agents
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : experimental
--
-- This module implements the gRPC transport for Zentinel agents.
-- It provides a bidirectional streaming interface for communication
-- with the Zentinel proxy over TCP.
--
-- = Protocol
--
-- The gRPC transport uses bidirectional streaming over TCP with
-- length-prefixed framing (v1 JSON encoding):
--
-- * Proxy streams 'ProxyMessage' to the agent
-- * Agent streams 'AgentMessage' back
--
-- Messages are framed identically to the UDS transport: a 4-byte
-- big-endian length prefix followed by the payload (v1 JSON encoding).
--
-- = Connection Lifecycle
--
-- 1. Proxy connects to the gRPC TCP endpoint
-- 2. Proxy sends a 'ProxyMsgHandshake' message
-- 3. Agent responds with 'AgentMsgHandshakeResponse' containing capabilities
-- 4. Bidirectional message loop: proxy sends events, agent sends responses
-- 5. Connection closes on shutdown, drain, or error
--
-- = Usage
--
-- @
-- runGrpcServer config handler
-- @
--
-- = Note
--
-- This module uses JSON-over-TCP with length-prefixed framing (v1 JSON
-- encoding) as the wire format. This provides wire compatibility with the
-- protocol while keeping the implementation straightforward. For production deployments requiring
-- native gRPC\/HTTP2 framing, generate Haskell types from the .proto files
-- using @proto3-suite@ and adapt the handlers accordingly.
module Zentinel.Agent.Transport.GRPC
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
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, catch, finally, throwIO)
import Control.Monad (forever, void)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode, toJSON)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word16, Word32, Word64)
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString qualified as SBS
import Zentinel.Agent.Handler
import Zentinel.Agent.Internal.Logging
import Zentinel.Agent.Types
import UnliftIO (MonadUnliftIO)

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
  | GrpcFramingError !String
  | GrpcMessageTooLarge !Int !Int
  deriving stock (Show, Eq)

instance Exception GrpcError

-- | Active gRPC connection state
data GrpcConnection = GrpcConnection
  { grpcConnSocket :: !Socket
  , grpcConnPeerId :: !Text
  , grpcConnPeerVersion :: !Text
  , grpcConnPeerAddr :: !SockAddr
  }

-- | Run the gRPC server
--
-- Starts a TCP server that accepts connections and handles the Zentinel
-- bidirectional streaming protocol. Each connection goes through a handshake
-- phase followed by a message loop that routes proxy events to the
-- appropriate 'AgentHandler' methods.
--
-- The server enforces a maximum connection limit specified by
-- 'grpcMaxConnections'. When the limit is reached, new connections are
-- delayed until an existing connection closes.
--
-- This function blocks until the server is shut down.
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
      host = T.unpack (hpHost addr)

  logInfo logger $
    "gRPC server starting on "
      <> hpHost addr
      <> ":"
      <> T.pack (show (hpPort addr))

  -- Track active connections
  activeConns <- newTVarIO (0 :: Int)

  -- Resolve bind address
  let hints =
        defaultHints
          { addrFlags = [AI_PASSIVE]
          , addrSocketType = Stream
          }
  addrInfos <- getAddrInfo (Just hints) (Just host) (Just (show (hpPort addr)))

  case addrInfos of
    [] -> throwIO (GrpcBindError $ "Cannot resolve address: " <> host <> ":" <> show (hpPort addr))
    (addrInfo : _) -> do
      -- Create TCP socket
      sock <- socket (addrFamily addrInfo) Stream defaultProtocol
      setSocketOption sock ReuseAddr 1

      -- Bind and listen
      bind sock (addrAddress addrInfo)
        `catch` \(e :: SomeException) ->
          throwIO (GrpcBindError $ "Failed to bind: " <> show e)

      listen sock (min (grpcMaxConnections config) 128)

      logInfo logger $
        "gRPC server listening on "
          <> hpHost addr
          <> ":"
          <> T.pack (show (hpPort addr))

      -- Accept loop
      let acceptLoop = forever $ do
            -- Check connection limit
            canAccept <- atomically $ do
              n <- readTVar activeConns
              if n < grpcMaxConnections config
                then do
                  writeTVar activeConns (n + 1)
                  pure True
                else pure False

            if canAccept
              then do
                (clientSock, clientAddr) <- accept sock
                logDebug logger $
                  "Accepted connection from " <> T.pack (show clientAddr)
                void $
                  forkIO $
                    handleGrpcConnection config runM caps clientSock clientAddr
                      `finally` do
                        atomically (modifyTVar' activeConns (subtract 1))
                        logDebug logger $
                          "Connection slot released for " <> T.pack (show clientAddr)
              else do
                -- Wait before retrying when at capacity
                threadDelay 10000

      acceptLoop `finally` do
        logInfo logger "gRPC server shutting down"
        close sock

-- | Handle a single gRPC connection
--
-- Performs the handshake protocol and then enters the message loop.
-- Any errors during handshake or message processing are caught and logged.
handleGrpcConnection ::
  forall m.
  (MonadUnliftIO m, AgentHandler m) =>
  GrpcServerConfig ->
  (forall a. m a -> IO a) ->
  AgentCapabilities ->
  Socket ->
  SockAddr ->
  IO ()
handleGrpcConnection config runM caps clientSock clientAddr = do
  let logger = grpcLogAction config

  -- Perform handshake
  handshakeResult <- performHandshake config logger caps clientSock clientAddr

  case handshakeResult of
    Left err -> do
      logError logger $ "Handshake failed from " <> T.pack (show clientAddr) <> ": " <> T.pack (show err)
      close clientSock
    Right conn -> do
      logInfo logger $
        "gRPC connection established with "
          <> grpcConnPeerId conn
          <> " (v"
          <> grpcConnPeerVersion conn
          <> ")"

      -- Message handling loop
      messageLoop config runM conn
        `catch` \(e :: SomeException) -> do
          logWarn logger $
            "gRPC connection error with "
              <> grpcConnPeerId conn
              <> ": "
              <> T.pack (show e)
        `finally` do
          logInfo logger $ "gRPC connection closed: " <> grpcConnPeerId conn
          close (grpcConnSocket conn)

-- | Perform handshake with the connecting proxy
--
-- Reads the first message from the connection, which must be a
-- 'ProxyMsgHandshake'. Negotiates protocol version and responds
-- with the agent's capabilities.
performHandshake ::
  GrpcServerConfig ->
  LogAction IO LogMessage ->
  AgentCapabilities ->
  Socket ->
  SockAddr ->
  IO (Either GrpcError GrpcConnection)
performHandshake config logger caps sock clientAddr = do
  -- Read the first framed message
  msgResult <- readGrpcMessage config sock
  case msgResult of
    Left err -> pure $ Left err
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

          -- Build and send handshake response
          let response =
                HandshakeResponse
                  { hsRespProtocolVersion = negotiatedVersion
                  , hsRespCapabilities = caps
                  , hsRespSuccess = True
                  , hsRespError = Nothing
                  }
              agentMsg = AgentMsgHandshakeResponse response

          writeGrpcMessage sock agentMsg

          pure $
            Right
              GrpcConnection
                { grpcConnSocket = sock
                , grpcConnPeerId = hsReqProxyId req
                , grpcConnPeerVersion = hsReqProxyVersion req
                , grpcConnPeerAddr = clientAddr
                }
        _ ->
          pure $ Left (GrpcHandshakeError "Expected handshake message as first message")

-- | Main message handling loop
--
-- Reads proxy messages from the connection, routes them to the
-- appropriate 'AgentHandler' method, and sends back responses.
-- Continues until the connection is closed or an error occurs.
messageLoop ::
  forall m.
  (MonadUnliftIO m, AgentHandler m) =>
  GrpcServerConfig ->
  (forall a. m a -> IO a) ->
  GrpcConnection ->
  IO ()
messageLoop config runM conn = do
  let sock = grpcConnSocket conn
      logger = grpcLogAction config

  forever $ do
    -- Read next message
    msgResult <- readGrpcMessage config sock
    case msgResult of
      Left GrpcConnectionClosed -> do
        logDebug logger $ "Connection closed by peer: " <> grpcConnPeerId conn
        throwIO GrpcConnectionClosed
      Left err -> do
        logError logger $ "Protocol error: " <> T.pack (show err)
        throwIO err
      Right (proxyMsg :: ProxyMessage) -> do
        -- Route message to handler and get optional response
        responseMsg <- handleMessage runM proxyMsg
        case responseMsg of
          Just msg -> writeGrpcMessage sock msg
          Nothing -> pure ()

-- | Route a proxy message to the appropriate handler method
--
-- This implements the same routing logic as the UDS transport,
-- mapping each 'ProxyMessage' variant to the corresponding
-- 'AgentHandler' method and wrapping the result in an 'AgentMessage'.
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
    -- Handshake should have already been handled during connection setup
    pure Nothing

--------------------------------------------------------------------------------
-- Wire format: length-prefixed framing (v1 JSON encoding)
--------------------------------------------------------------------------------

-- | Read a length-prefixed message from a gRPC socket (v1 JSON encoding)
--
-- Wire format: 4-byte big-endian length prefix followed by JSON payload.
-- Validates the message size against 'grpcMaxMessageSize'.
readGrpcMessage :: FromJSON a => GrpcServerConfig -> Socket -> IO (Either GrpcError a)
readGrpcMessage config sock = do
  -- Read 4-byte length header
  headerResult <- recvExact sock 4
  case headerResult of
    Left err -> pure (Left err)
    Right header -> do
      let len = decodeBE32 header
      -- Validate length against configured maximum
      if fromIntegral len > grpcMaxMessageSize config
        then pure $ Left (GrpcMessageTooLarge (fromIntegral len) (grpcMaxMessageSize config))
        else
          if len == 0
            then pure $ Left (GrpcParseError "Empty message")
            else do
              -- Read payload
              payloadResult <- recvExact sock (fromIntegral len)
              case payloadResult of
                Left err -> pure (Left err)
                Right payload ->
                  case eitherDecodeStrict payload of
                    Left err -> pure $ Left (GrpcParseError err)
                    Right val -> pure (Right val)

-- | Write a length-prefixed message to a gRPC socket (v1 JSON encoding)
--
-- Encodes the message as JSON, prepends a 4-byte big-endian length
-- header, and sends the complete frame.
writeGrpcMessage :: ToJSON a => Socket -> a -> IO ()
writeGrpcMessage sock msg = do
  let payload = LBS.toStrict (encode msg)
      len = BS.length payload
      header = encodeBE32 (fromIntegral len)
      frame = header <> payload
  sendAll sock frame

-- | Receive exactly @n@ bytes from a socket
--
-- Accumulates data from the socket until exactly @n@ bytes have been
-- received. Returns 'GrpcConnectionClosed' if the peer closes the
-- connection before all bytes are received.
recvExact :: Socket -> Int -> IO (Either GrpcError ByteString)
recvExact sock n = go n []
  where
    go 0 chunks = pure $ Right (BS.concat (reverse chunks))
    go remaining chunks = do
      chunk <- SBS.recv sock (min remaining 65536)
      if BS.null chunk
        then pure $ Left GrpcConnectionClosed
        else go (remaining - BS.length chunk) (chunk : chunks)

-- | Send all bytes to a socket
--
-- Repeatedly calls send until all bytes have been transmitted.
-- Throws 'GrpcConnectionClosed' if the socket closes mid-send.
sendAll :: Socket -> ByteString -> IO ()
sendAll sock bs
  | BS.null bs = pure ()
  | otherwise = do
      sent <- SBS.send sock bs
      if sent == 0
        then throwIO GrpcConnectionClosed
        else sendAll sock (BS.drop sent bs)

-- | Encode a 32-bit value as 4 bytes in big-endian order
encodeBE32 :: Word32 -> ByteString
encodeBE32 n =
  BS.pack
    [ fromIntegral (shiftR n 24 .&. 0xFF)
    , fromIntegral (shiftR n 16 .&. 0xFF)
    , fromIntegral (shiftR n 8 .&. 0xFF)
    , fromIntegral (n .&. 0xFF)
    ]

-- | Decode 4 bytes from big-endian order to a 32-bit value
decodeBE32 :: ByteString -> Word32
decodeBE32 bs
  | BS.length bs < 4 = 0
  | otherwise =
      let [b0, b1, b2, b3] = map fromIntegral (BS.unpack (BS.take 4 bs)) :: [Word32]
       in shiftL b0 24 .|. shiftL b1 16 .|. shiftL b2 8 .|. b3

-- | Get current time in milliseconds since Unix epoch
getCurrentTimeMs :: IO Word64
getCurrentTimeMs = do
  t <- getPOSIXTime
  pure $ round (t * 1000)
