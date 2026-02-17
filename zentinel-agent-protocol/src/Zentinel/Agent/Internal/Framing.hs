-- |
-- Module      : Zentinel.Agent.Internal.Framing
-- Description : Message framing for UDS transport
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : internal
--
-- This module implements length-prefixed message framing for the UDS transport.
-- Messages are framed with a 4-byte big-endian length prefix followed by the
-- JSON payload.
--
-- Wire format:
--
-- @
-- +----------------+------------------+
-- | Length (4 bytes) | JSON Payload     |
-- | big-endian u32   | (length bytes)   |
-- +----------------+------------------+
-- @
module Zentinel.Agent.Internal.Framing
  ( -- * Framing
    frameMessage
  , unframeMessage
  , readFramedMessage
  , writeFramedMessage

    -- * Errors
  , FramingError (..)

    -- * Constants
  , maxFrameSize
  ) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word32, Word8)
import Network.Socket (Socket)
import Network.Socket.ByteString qualified as SBS
import Zentinel.Agent.Types (maxUdsMessageSize)
import UnliftIO (MonadIO, liftIO, throwIO)

-- | Maximum frame size (16MB, matches Rust implementation)
maxFrameSize :: Int
maxFrameSize = maxUdsMessageSize

-- | Framing errors
data FramingError
  = -- | Frame exceeds maximum size
    FrameTooLarge
      { frameSize :: !Int
      , maxSize :: !Int
      }
  | -- | Unexpected end of stream
    UnexpectedEof
  | -- | Invalid frame header
    InvalidHeader !ByteString
  | -- | JSON parse error
    ParseError !String
  | -- | Socket error
    SocketError !String
  deriving stock (Show, Eq)

instance Exception FramingError

-- | Frame a message with length prefix
frameMessage :: ToJSON a => a -> ByteString
frameMessage msg =
  let payload = LBS.toStrict (encode msg)
      len = BS.length payload
      header = encodeLength (fromIntegral len)
   in header <> payload

-- | Unframe a message (extract payload from length-prefixed frame)
unframeMessage :: FromJSON a => ByteString -> Either FramingError a
unframeMessage bs
  | BS.length bs < 4 = Left (InvalidHeader bs)
  | otherwise =
      let (header, rest) = BS.splitAt 4 bs
          len = decodeLength header
       in if len > fromIntegral maxFrameSize
            then Left (FrameTooLarge (fromIntegral len) maxFrameSize)
            else
              if BS.length rest < fromIntegral len
                then Left UnexpectedEof
                else case eitherDecodeStrict (BS.take (fromIntegral len) rest) of
                  Left err -> Left (ParseError err)
                  Right val -> Right val

-- | Read a framed message from a socket
readFramedMessage :: (MonadIO m, FromJSON a) => Socket -> m (Either FramingError a)
readFramedMessage sock = liftIO $ do
  -- Read 4-byte length header
  headerResult <- recvExact sock 4
  case headerResult of
    Left err -> pure (Left err)
    Right header -> do
      let len = decodeLength header
      -- Validate length
      if len > fromIntegral maxFrameSize
        then pure $ Left (FrameTooLarge (fromIntegral len) maxFrameSize)
        else
          if len == 0
            then pure $ Left (ParseError "Empty message")
            else do
              -- Read payload
              payloadResult <- recvExact sock (fromIntegral len)
              case payloadResult of
                Left err -> pure (Left err)
                Right payload ->
                  case eitherDecodeStrict payload of
                    Left err -> pure $ Left (ParseError err)
                    Right val -> pure (Right val)

-- | Write a framed message to a socket
writeFramedMessage :: (MonadIO m, ToJSON a) => Socket -> a -> m ()
writeFramedMessage sock msg = liftIO $ do
  let framed = frameMessage msg
  sendAll sock framed

-- | Receive exactly n bytes from a socket
recvExact :: Socket -> Int -> IO (Either FramingError ByteString)
recvExact sock n = go n []
  where
    go 0 chunks = pure $ Right (BS.concat (reverse chunks))
    go remaining chunks = do
      chunk <- SBS.recv sock (min remaining 65536)
      if BS.null chunk
        then pure $ Left UnexpectedEof
        else go (remaining - BS.length chunk) (chunk : chunks)

-- | Send all bytes to a socket
sendAll :: Socket -> ByteString -> IO ()
sendAll sock bs
  | BS.null bs = pure ()
  | otherwise = do
      sent <- SBS.send sock bs
      if sent == 0
        then throwIO (SocketError "Connection closed")
        else sendAll sock (BS.drop sent bs)

-- | Encode a 32-bit length as 4 bytes (big-endian)
encodeLength :: Word32 -> ByteString
encodeLength n =
  BS.pack
    [ fromIntegral (shiftR n 24 .&. 0xFF)
    , fromIntegral (shiftR n 16 .&. 0xFF)
    , fromIntegral (shiftR n 8 .&. 0xFF)
    , fromIntegral (n .&. 0xFF)
    ]

-- | Decode 4 bytes as a 32-bit length (big-endian)
decodeLength :: ByteString -> Word32
decodeLength bs
  | BS.length bs < 4 = 0
  | otherwise =
      let [b0, b1, b2, b3] = map fromIntegral (BS.unpack (BS.take 4 bs)) :: [Word32]
       in shiftL b0 24 .|. shiftL b1 16 .|. shiftL b2 8 .|. b3
