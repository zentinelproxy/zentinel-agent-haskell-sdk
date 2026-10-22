-- |
-- Module      : Zentinel.Agent.TypesSpec
-- Description : Tests for protocol types JSON serialization
module Zentinel.Agent.TypesSpec (spec) where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.HashMap.Strict qualified as HM
import Zentinel.Agent.Types
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Test.Hspec

spec :: Spec
spec = do
  describe "EventType" $ do
    it "serializes to snake_case" $ do
      encode RequestHeaders `shouldBe` "\"request_headers\""
      encode RequestBodyChunk `shouldBe` "\"request_body_chunk\""
      encode GuardrailInspect `shouldBe` "\"guardrail_inspect\""

    it "round-trips through JSON" $ do
      let events = [minBound .. maxBound] :: [EventType]
      mapM_ (\e -> decode (encode e) `shouldBe` Just e) events

  describe "Decision" $ do
    -- serde's externally tagged form, which is what the proxy parses
    it "serializes Allow as the bare string" $ do
      encode Allow `shouldBe` "\"allow\""

    it "serializes Block externally tagged" $ do
      let d = Block 403 (Just "Forbidden") Nothing
      decode (encode d)
        `shouldBe` Just (object ["block" .= object ["status" .= (403 :: Int), "body" .= ("Forbidden" :: String), "headers" .= Null]])

    it "serializes Redirect externally tagged" $ do
      let d = Redirect "https://example.com" 302
      decode (encode d)
        `shouldBe` Just (object ["redirect" .= object ["url" .= ("https://example.com" :: String), "status" .= (302 :: Int)]])

    it "round-trips Allow" $ do
      decode (encode Allow) `shouldBe` Just Allow

    it "round-trips Block" $ do
      let d = Block 403 (Just "Forbidden") (Just (HM.singleton "X-Error" "true"))
      decode (encode d) `shouldBe` Just d

  describe "AgentCapabilities" $ do
    it "serializes with all fields" $ do
      let caps = defaultCapabilities "test-agent"
      encode caps `encodesWith` "\"protocol_version\":2"
      encode caps `encodesWith` "\"name\":\"test-agent\""

    it "round-trips through JSON" $ do
      let caps = defaultCapabilities "test-agent"
      decode (encode caps) `shouldBe` Just caps

  describe "HealthState" $ do
    it "serializes Healthy correctly" $ do
      encode StateHealthy `encodesWith` "\"status\":\"healthy\""

    it "serializes Degraded correctly" $ do
      let state = StateDegraded ["streaming"] 1.5
      encode state `encodesWith` "\"status\":\"degraded\""
      encode state `encodesWith` "\"timeout_multiplier\":1.5"

    it "serializes Draining correctly" $ do
      let state = StateDraining (Just 30000)
      encode state `encodesWith` "\"status\":\"draining\""
      encode state `encodesWith` "\"eta_ms\":30000"

    it "serializes Unhealthy correctly" $ do
      let state = StateUnhealthy "OOM" True
      encode state `encodesWith` "\"status\":\"unhealthy\""
      encode state `encodesWith` "\"recoverable\":true"

    it "round-trips all states" $ do
      let states =
            [ StateHealthy
            , StateDegraded ["a", "b"] 2.0
            , StateDraining Nothing
            , StateDraining (Just 5000)
            , StateUnhealthy "error" False
            ]
      mapM_ (\s -> decode (encode s) `shouldBe` Just s) states

  describe "HeaderOp" $ do
    it "serializes Set correctly" $ do
      let op = HeaderSet "X-Custom" "value"
      encode op `encodesWith` "\"set\""
      encode op `encodesWith` "\"name\":\"X-Custom\""

    it "serializes Add correctly" $ do
      let op = HeaderAdd "X-Custom" "value"
      encode op `encodesWith` "\"add\""

    it "serializes Remove correctly" $ do
      let op = HeaderRemove "X-Custom"
      encode op `encodesWith` "\"remove\""

    it "round-trips all operations" $ do
      let ops =
            [ HeaderSet "X-A" "1"
            , HeaderAdd "X-B" "2"
            , HeaderRemove "X-C"
            ]
      mapM_ (\o -> decode (encode o) `shouldBe` Just o) ops

  describe "AgentResponse" $ do
    it "uses default values for missing fields" $ do
      let json = "{\"decision\":\"allow\"}"
      let resp = decode json :: Maybe AgentResponse
      resp `shouldSatisfy` maybe False (\r -> respDecision r == Allow)

    it "decodes a proxy block response" $ do
      let json = "{\"version\":2,\"decision\":{\"block\":{\"status\":403,\"body\":\"no\",\"headers\":null}}}"
      fmap respDecision (decode json) `shouldBe` Just (Block 403 (Just "no") Nothing)

  describe "events" $ do
    it "decode the proxy's request headers event" $ do
      let json = "{\"metadata\":{\"correlation_id\":\"c1\",\"request_id\":\"r1\",\"client_ip\":\"127.0.0.1\",\"client_port\":5000,\"server_name\":null,\"protocol\":\"HTTP/1.1\",\"tls_version\":null,\"tls_cipher\":null,\"route_id\":\"default\",\"upstream_id\":null,\"timestamp\":\"2026-10-22T00:00:00Z\"},\"method\":\"GET\",\"uri\":\"/x\",\"headers\":{\"host\":[\"a\"]}}"
      fmap reqHdrUri (decode json :: Maybe RequestHeadersEvent) `shouldBe` Just "/x"

    it "decode the proxy's request complete event" $ do
      let json = "{\"correlation_id\":\"c1\",\"status\":200,\"duration_ms\":3,\"request_body_size\":0,\"response_body_size\":5,\"upstream_attempts\":1,\"error\":null}"
      fmap reqCompStatus (decode json :: Maybe RequestCompleteEvent) `shouldBe` Just 200

  describe "HistogramBucket" $ do
    it "serializes +Inf correctly" $ do
      let bucket = HistogramBucket (1 / 0) 100
      encode bucket `encodesWith` "\"+Inf\""

    it "round-trips +Inf" $ do
      let bucket = HistogramBucket (1 / 0) 100
      let decoded = decode (encode bucket) :: Maybe HistogramBucket
      decoded `shouldSatisfy` maybe False (isInfinite . bucketLe)

-- | The encoded JSON contains the given text.
encodesWith :: LBS.ByteString -> BS.ByteString -> Expectation
encodesWith actual needle = LBS.toStrict actual `shouldSatisfy` BS.isInfixOf needle
