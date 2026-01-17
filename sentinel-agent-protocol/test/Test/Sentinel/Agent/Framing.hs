-- |
-- Module      : Test.Sentinel.Agent.Framing
-- Description : Tests for message framing
module Test.Sentinel.Agent.Framing (spec) where

import Data.Aeson (Value (..))
import Data.ByteString qualified as BS
import Sentinel.Agent.Internal.Framing
import Sentinel.Agent.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "frameMessage" $ do
    it "produces 4-byte length prefix" $ do
      let framed = frameMessage Allow
      BS.length framed `shouldSatisfy` (> 4)

    it "encodes length in big-endian" $ do
      let framed = frameMessage Allow
          len = decodeLen (BS.take 4 framed)
          payloadLen = BS.length framed - 4
      len `shouldBe` fromIntegral payloadLen

  describe "unframeMessage" $ do
    it "round-trips simple values" $ do
      let original = Allow :: Decision
          framed = frameMessage original
      unframeMessage framed `shouldBe` Right original

    it "round-trips complex values" $ do
      let original =
            AgentResponse
              { respVersion = 1
              , respDecision = Block 403 (Just "Forbidden") Nothing
              , respRequestHeaders = [HeaderSet "X-Test" "value"]
              , respResponseHeaders = []
              , respRoutingMetadata = mempty
              , respAudit = AuditMetadata ["tag"] [] Nothing [] mempty
              , respNeedsMore = False
              , respRequestBodyMutation = Nothing
              , respResponseBodyMutation = Nothing
              , respWebSocketDecision = Nothing
              }
          framed = frameMessage original
      unframeMessage framed `shouldBe` Right original

    it "fails on truncated header" $ do
      let result = unframeMessage (BS.pack [0, 0, 0]) :: Either FramingError Decision
      result `shouldSatisfy` isInvalidHeader

    it "fails on truncated payload" $ do
      let framed = frameMessage Allow
          truncated = BS.take (BS.length framed - 2) framed
      let result = unframeMessage truncated :: Either FramingError Decision
      result `shouldSatisfy` isUnexpectedEof

    it "fails on oversized frame" $ do
      -- Create a frame claiming to be very large
      let fakeHeader = BS.pack [0xFF, 0xFF, 0xFF, 0xFF]
          fakePayload = BS.pack [0x7B, 0x7D]  -- {}
          fakeFrame = fakeHeader <> fakePayload
      let result = unframeMessage fakeFrame :: Either FramingError Decision
      result `shouldSatisfy` isFrameTooLarge

  describe "maxFrameSize" $ do
    it "matches protocol constant" $ do
      maxFrameSize `shouldBe` maxUdsMessageSize

-- Helper to decode length from bytes
decodeLen :: BS.ByteString -> Int
decodeLen bs
  | BS.length bs < 4 = 0
  | otherwise =
      let [b0, b1, b2, b3] = map fromIntegral (BS.unpack (BS.take 4 bs))
       in b0 * 256 ^ 3 + b1 * 256 ^ 2 + b2 * 256 + b3

-- Error predicates
isInvalidHeader :: Either FramingError a -> Bool
isInvalidHeader (Left (InvalidHeader _)) = True
isInvalidHeader _ = False

isUnexpectedEof :: Either FramingError a -> Bool
isUnexpectedEof (Left UnexpectedEof) = True
isUnexpectedEof _ = False

isFrameTooLarge :: Either FramingError a -> Bool
isFrameTooLarge (Left (FrameTooLarge _ _)) = True
isFrameTooLarge _ = False
