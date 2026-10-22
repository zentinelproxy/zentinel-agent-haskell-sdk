{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}


-- |
-- Module      : Zentinel.Agent.WireSpec
-- Description : The v2 UDS framing and the transport-built payloads
module Zentinel.Agent.WireSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HM
import Test.Hspec
import Zentinel.Agent.Types
import Zentinel.Agent.Wire

spec :: Spec
spec = do
  describe "encodeFrame" $ do
    it "counts the type byte in the length" $ do
      encodeFrame MsgAgentResponse "{}" `shouldBe` Right (BS.pack [0, 0, 0, 3, 0x20, 0x7B, 0x7D])

    it "round-trips through decodeFrame" $ do
      let Right framed = encodeFrame MsgPong "abc"
      decodeFrame (framed <> "rest") `shouldBe` Right (Just (0x42, "abc", "rest"))

    it "rejects payloads over 16 MiB" $ do
      encodeFrame MsgAgentResponse (BS.replicate maxFrameSize 0) `shouldSatisfy` either (const True) (const False)

  describe "decodeFrame" $ do
    it "waits for a whole frame" $ do
      decodeFrame (BS.pack [0, 0, 0, 5, 0x20, 1]) `shouldBe` Right Nothing

    it "rejects a zero length" $ do
      decodeFrame (BS.pack [0, 0, 0, 0]) `shouldBe` Left EmptyFrame

  describe "message types" $ do
    it "use the proxy's type bytes" $ do
      map messageTypeByte [MsgHandshakeRequest, MsgHandshakeResponse, MsgRequestHeaders, MsgConfigure, MsgAgentResponse, MsgCancel, MsgPing, MsgPong]
        `shouldBe` [0x01, 0x02, 0x10, 0x17, 0x20, 0x40, 0x41, 0x42]

    it "round-trip through their bytes" $ do
      mapM_ (\t -> messageTypeFromByte (messageTypeByte t) `shouldBe` Just t) [minBound .. maxBound]

  describe "udsHandshakeResponse" $ do
    let caps = (defaultCapabilities "t") {capSupportedEvents = [RequestHeaders, RequestBodyChunk, ResponseHeaders]}
        Object reply = udsHandshakeResponse caps [2]
        Just (Object c) = KM.lookup "capabilities" reply

    it "has every field of UdsHandshakeResponse" $ do
      KM.keys reply `shouldMatchList` ["protocol_version", "capabilities", "success", "error", "encoding"]
      KM.lookup "protocol_version" reply `shouldBe` Just (Number 2)
      KM.lookup "success" reply `shouldBe` Just (Bool True)
      KM.lookup "error" reply `shouldBe` Just Null
      KM.lookup "encoding" reply `shouldBe` Just (String "json")

    it "has every field of UdsCapabilities, events as UDS numbers" $ do
      KM.keys c `shouldMatchList` ["agent_id", "name", "version", "supported_events", "features", "limits"]
      KM.lookup "supported_events" c `shouldBe` Just (toArray [1, 2, 3])
      let Just (Object f) = KM.lookup "features" c
          Just (Object l) = KM.lookup "limits" c
      KM.keys f
        `shouldMatchList` [ "streaming_body", "websocket", "guardrails", "config_push", "metrics_export"
                          , "concurrent_requests", "cancellation", "flow_control", "health_reporting" ]
      KM.keys l `shouldMatchList` ["max_body_size", "max_concurrency", "preferred_chunk_size"]
      Null `notElem` (KM.elems f <> KM.elems l) `shouldBe` True

    it "refuses a proxy that does not offer v2" $ do
      let Object refused = udsHandshakeResponse caps [1]
      KM.lookup "success" refused `shouldBe` Just (Bool False)
      KM.lookup "error" refused `shouldNotBe` Just Null

  describe "correlation ids" $ do
    it "are read from the top level or from metadata" $ do
      correlationIdOf (object ["correlation_id" .= ("a" :: String)]) `shouldBe` "a"
      correlationIdOf (object ["metadata" .= object ["correlation_id" .= ("b" :: String)]]) `shouldBe` "b"

    it "go into audit.custom" $ do
      auditCustom (respAudit (withCorrelationId "c" allow)) `shouldBe` HM.singleton "correlation_id" (String "c")

  describe "configureFromPayload" $ do
    it "takes config and config_version from the envelope" $ do
      configureFromPayload (object ["config" .= object ["a" .= (1 :: Int)], "config_version" .= ("v1" :: String), "correlation_id" .= ("x" :: String)])
        `shouldBe` (object ["a" .= (1 :: Int)], Just "v1")
  where
    toArray :: [Int] -> Value
    toArray = \xs -> case object ["x" .= xs] of
      Object o | Just v <- KM.lookup "x" o -> v
      v -> v
