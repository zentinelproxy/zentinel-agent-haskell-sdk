-- |
-- Module      : Zentinel.Agent.Wire
-- Description : Agent protocol v2 over a Unix domain socket: framing and payloads
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : experimental
--
-- Every message on the socket is one frame:
--
-- @
-- +-------------------+----------+-----------------+
-- | length (4, BE)    | type (1) | payload (JSON)  |
-- +-------------------+----------+-----------------+
-- @
--
-- @length@ counts the type byte and the payload. This matches
-- @zentinel_agent_protocol::v2::uds@ in the proxy. The payloads are the serde
-- JSON forms of the proxy's types; this module builds the ones whose shape the
-- transport, not the agent, is responsible for: the handshake reply, and the
-- correlation id every 'AgentResponse' must carry.
module Zentinel.Agent.Wire
  ( -- * Message types
    MessageType (..)
  , messageTypeByte
  , messageTypeFromByte
  , eventMessageType

    -- * Framing
  , FramingError (..)
  , maxFrameSize
  , encodeFrame
  , decodeFrame
  , readFrame
  , writeFrame

    -- * Handshake
  , udsEventNumber
  , udsCapabilities
  , udsHandshakeResponse

    -- * Responses
  , correlationIdOf
  , withCorrelationId
  , configureFromPayload
  ) where

import Control.Exception (Exception)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.Word (Word32, Word8)
import Network.Socket (Socket)
import Network.Socket.ByteString qualified as SBS
import Zentinel.Agent.Types

-- | The type byte of a v2 UDS frame.
data MessageType
  = MsgHandshakeRequest
  | MsgHandshakeResponse
  | MsgRequestHeaders
  | MsgRequestBodyChunk
  | MsgResponseHeaders
  | MsgResponseBodyChunk
  | MsgRequestComplete
  | MsgWebSocketFrame
  | MsgGuardrailInspect
  | MsgConfigure
  | MsgAgentResponse
  | MsgHealthStatus
  | MsgMetricsReport
  | MsgConfigUpdateRequest
  | MsgFlowControl
  | MsgCancel
  | MsgPing
  | MsgPong
  deriving stock (Eq, Show, Ord, Bounded, Enum)

-- | The byte a message type is sent as.
messageTypeByte :: MessageType -> Word8
messageTypeByte = \case
  MsgHandshakeRequest -> 0x01
  MsgHandshakeResponse -> 0x02
  MsgRequestHeaders -> 0x10
  MsgRequestBodyChunk -> 0x11
  MsgResponseHeaders -> 0x12
  MsgResponseBodyChunk -> 0x13
  MsgRequestComplete -> 0x14
  MsgWebSocketFrame -> 0x15
  MsgGuardrailInspect -> 0x16
  MsgConfigure -> 0x17
  MsgAgentResponse -> 0x20
  MsgHealthStatus -> 0x30
  MsgMetricsReport -> 0x31
  MsgConfigUpdateRequest -> 0x32
  MsgFlowControl -> 0x33
  MsgCancel -> 0x40
  MsgPing -> 0x41
  MsgPong -> 0x42

-- | The message type of a byte, if it is one.
messageTypeFromByte :: Word8 -> Maybe MessageType
messageTypeFromByte b = lookup b [(messageTypeByte t, t) | t <- [minBound .. maxBound]]

-- | The event a message type carries, for the event messages.
eventMessageType :: MessageType -> Maybe EventType
eventMessageType = \case
  MsgRequestHeaders -> Just RequestHeaders
  MsgRequestBodyChunk -> Just RequestBodyChunk
  MsgResponseHeaders -> Just ResponseHeaders
  MsgResponseBodyChunk -> Just ResponseBodyChunk
  MsgRequestComplete -> Just RequestComplete
  MsgWebSocketFrame -> Just WebSocketFrame
  MsgGuardrailInspect -> Just GuardrailInspect
  MsgConfigure -> Just Configure
  _ -> Nothing

-- | Framing errors
data FramingError
  = -- | Frame exceeds the maximum size
    FrameTooLarge !Int !Int
  | -- | A frame with no type byte
    EmptyFrame
  | -- | The peer closed the connection in the middle of a frame
    UnexpectedEof
  deriving stock (Show, Eq)

instance Exception FramingError

-- | Largest frame the proxy sends or accepts (16 MiB).
maxFrameSize :: Int
maxFrameSize = maxUdsMessageSize

-- | Frame a payload.
encodeFrame :: MessageType -> ByteString -> Either FramingError ByteString
encodeFrame t payload
  | len > maxFrameSize = Left (FrameTooLarge len maxFrameSize)
  | otherwise = Right (encodeLength (fromIntegral len) <> BS.singleton (messageTypeByte t) <> payload)
  where
    len = BS.length payload + 1

-- | Split one frame off the front of a buffer: the type byte, the payload
-- and the rest. 'Nothing' if the buffer does not hold a whole frame yet.
decodeFrame :: ByteString -> Either FramingError (Maybe (Word8, ByteString, ByteString))
decodeFrame bs
  | BS.length bs < 4 = Right Nothing
  | len == 0 = Left EmptyFrame
  | len > maxFrameSize = Left (FrameTooLarge len maxFrameSize)
  | BS.length body < len = Right Nothing
  | otherwise = Right (Just (BS.head body, BS.take (len - 1) (BS.tail body), BS.drop len body))
  where
    len = fromIntegral (decodeLength (BS.take 4 bs))
    body = BS.drop 4 bs

-- | Read one frame. 'Nothing' when the peer closed the connection between
-- frames.
readFrame :: Socket -> IO (Either FramingError (Maybe (Word8, ByteString)))
readFrame sock = do
  header <- recvExact sock 4
  case header of
    Nothing -> pure (Right Nothing)
    Just h -> do
      let len = fromIntegral (decodeLength h)
      if len == 0
        then pure (Left EmptyFrame)
        else
          if len > maxFrameSize
            then pure (Left (FrameTooLarge len maxFrameSize))
            else do
              body <- recvExact sock len
              pure $ case body of
                Nothing -> Left UnexpectedEof
                Just b -> Right (Just (BS.head b, BS.tail b))

-- | Write one frame. The caller serializes writes on a socket.
writeFrame :: Socket -> MessageType -> ByteString -> IO (Either FramingError ())
writeFrame sock t payload = traverse (SBS.sendAll sock) (encodeFrame t payload)

recvExact :: Socket -> Int -> IO (Maybe ByteString)
recvExact sock n = go n []
  where
    go 0 chunks = pure (Just (BS.concat (reverse chunks)))
    go remaining chunks = do
      chunk <- SBS.recv sock (min remaining 65536)
      if BS.null chunk
        then pure Nothing
        else go (remaining - BS.length chunk) (chunk : chunks)

encodeLength :: Word32 -> ByteString
encodeLength n =
  BS.pack
    [ fromIntegral (shiftR n 24 .&. 0xFF)
    , fromIntegral (shiftR n 16 .&. 0xFF)
    , fromIntegral (shiftR n 8 .&. 0xFF)
    , fromIntegral (n .&. 0xFF)
    ]

decodeLength :: ByteString -> Word32
decodeLength = BS.foldl' (\acc b -> shiftL acc 8 .|. fromIntegral b) 0

--------------------------------------------------------------------------------
-- Handshake
--------------------------------------------------------------------------------

-- | The number an event type has in the UDS handshake's @supported_events@.
udsEventNumber :: EventType -> Int
udsEventNumber = \case
  Configure -> 0
  RequestHeaders -> 1
  RequestBodyChunk -> 2
  ResponseHeaders -> 3
  ResponseBodyChunk -> 4
  RequestComplete -> 5
  WebSocketFrame -> 6
  GuardrailInspect -> 7

-- | The handshake reply's @capabilities@ (@UdsCapabilities@ in the proxy).
-- Every field, @features@ and @limits@ included, is required.
udsCapabilities :: AgentCapabilities -> Value
udsCapabilities caps =
  object
    [ "agent_id" .= capAgentId caps
    , "name" .= capName caps
    , "version" .= capVersion caps
    , "supported_events" .= map udsEventNumber (capSupportedEvents caps)
    , "features" .= capFeatures caps
    , "limits"
        .= object
          [ "max_body_size" .= limMaxBodySize lim
          , "max_concurrency" .= limMaxConcurrency lim
          , "preferred_chunk_size" .= limPreferredChunkSize lim
          ]
    ]
  where
    lim = capLimits caps

-- | The handshake reply (@UdsHandshakeResponse@) to a request offering
-- @versions@: success when protocol v2 is among them.
udsHandshakeResponse :: AgentCapabilities -> [Word32] -> Value
udsHandshakeResponse caps versions =
  object
    [ "protocol_version" .= protocolVersion
    , "capabilities" .= udsCapabilities caps
    , "success" .= ok
    , "error" .= if ok then Nothing else Just ("protocol v2 not offered by the proxy" :: Text)
    , "encoding" .= ("json" :: Text)
    ]
  where
    ok = protocolVersion `elem` versions

--------------------------------------------------------------------------------
-- Responses
--------------------------------------------------------------------------------

-- | The correlation id of an event payload: top level, or in @metadata@
-- (request headers).
correlationIdOf :: Value -> Text
correlationIdOf (Object o) =
  case KM.lookup "correlation_id" o of
    Just (String cid) | cid /= "" -> cid
    _ -> case KM.lookup "metadata" o of
      Just (Object m) | Just (String cid) <- KM.lookup "correlation_id" m -> cid
      _ -> ""
correlationIdOf _ = ""

-- | Put the request's correlation id into @audit.custom.correlation_id@,
-- which is how the proxy matches a response to its request.
withCorrelationId :: Text -> AgentResponse -> AgentResponse
withCorrelationId cid resp =
  resp
    { respAudit =
        (respAudit resp)
          { auditCustom = HM.insert "correlation_id" (A.toJSON cid) (auditCustom (respAudit resp))
          }
    }

-- | The configuration and its version from a Configure payload
-- (@{"config": ..., "config_version": ..., "correlation_id": ...}@).
configureFromPayload :: Value -> (Value, Maybe Text)
configureFromPayload (Object o) =
  ( case KM.lookup "config" o of
      Just v -> v
      Nothing -> Object (KM.delete "correlation_id" o)
  , case KM.lookup "config_version" o of
      Just (String v) -> Just v
      _ -> Nothing
  )
configureFromPayload v = (v, Nothing)
