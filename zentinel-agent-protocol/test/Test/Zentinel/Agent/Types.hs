-- |
-- Module      : Test.Zentinel.Agent.Types
-- Description : Tests for protocol types JSON serialization
module Test.Zentinel.Agent.Types (spec) where

import Data.Aeson (decode, encode)
import Data.HashMap.Strict qualified as HM
import Zentinel.Agent.Types
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
    it "serializes Allow correctly" $ do
      encode Allow `shouldContain` "\"type\":\"allow\""

    it "serializes Block correctly" $ do
      let d = Block 403 (Just "Forbidden") Nothing
      encode d `shouldContain` "\"type\":\"block\""
      encode d `shouldContain` "\"status\":403"
      encode d `shouldContain` "\"body\":\"Forbidden\""

    it "serializes Redirect correctly" $ do
      let d = Redirect "https://example.com" 302
      encode d `shouldContain` "\"type\":\"redirect\""
      encode d `shouldContain` "\"url\":\"https://example.com\""
      encode d `shouldContain` "\"status\":302"

    it "round-trips Allow" $ do
      decode (encode Allow) `shouldBe` Just Allow

    it "round-trips Block" $ do
      let d = Block 403 (Just "Forbidden") (Just (HM.singleton "X-Error" "true"))
      decode (encode d) `shouldBe` Just d

  describe "AgentCapabilities" $ do
    it "serializes with all fields" $ do
      let caps = defaultCapabilities "test-agent"
      encode caps `shouldContain` "\"protocol_version\":2"
      encode caps `shouldContain` "\"name\":\"test-agent\""

    it "round-trips through JSON" $ do
      let caps = defaultCapabilities "test-agent"
      decode (encode caps) `shouldBe` Just caps

  describe "HealthState" $ do
    it "serializes Healthy correctly" $ do
      encode StateHealthy `shouldContain` "\"status\":\"healthy\""

    it "serializes Degraded correctly" $ do
      let state = StateDegraded ["streaming"] 1.5
      encode state `shouldContain` "\"status\":\"degraded\""
      encode state `shouldContain` "\"timeout_multiplier\":1.5"

    it "serializes Draining correctly" $ do
      let state = StateDraining (Just 30000)
      encode state `shouldContain` "\"status\":\"draining\""
      encode state `shouldContain` "\"eta_ms\":30000"

    it "serializes Unhealthy correctly" $ do
      let state = StateUnhealthy "OOM" True
      encode state `shouldContain` "\"status\":\"unhealthy\""
      encode state `shouldContain` "\"recoverable\":true"

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
      encode op `shouldContain` "\"set\""
      encode op `shouldContain` "\"name\":\"X-Custom\""

    it "serializes Add correctly" $ do
      let op = HeaderAdd "X-Custom" "value"
      encode op `shouldContain` "\"add\""

    it "serializes Remove correctly" $ do
      let op = HeaderRemove "X-Custom"
      encode op `shouldContain` "\"remove\""

    it "round-trips all operations" $ do
      let ops =
            [ HeaderSet "X-A" "1"
            , HeaderAdd "X-B" "2"
            , HeaderRemove "X-C"
            ]
      mapM_ (\o -> decode (encode o) `shouldBe` Just o) ops

  describe "AgentResponse" $ do
    it "uses default values for missing fields" $ do
      let json = "{\"decision\":{\"type\":\"allow\"}}"
      let resp = decode json :: Maybe AgentResponse
      resp `shouldSatisfy` maybe False (\r -> respDecision r == Allow)

  describe "HistogramBucket" $ do
    it "serializes +Inf correctly" $ do
      let bucket = HistogramBucket (1 / 0) 100
      encode bucket `shouldContain` "\"+Inf\""

    it "round-trips +Inf" $ do
      let bucket = HistogramBucket (1 / 0) 100
      let decoded = decode (encode bucket) :: Maybe HistogramBucket
      decoded `shouldSatisfy` maybe False (isInfinite . bucketLe)

-- Helper to check if ByteString contains substring
shouldContain :: Show a => a -> String -> Expectation
shouldContain actual expected =
  show actual `shouldSatisfy` \s -> expected `isInfixOf` s
  where
    isInfixOf needle haystack = needle `elem` substrings haystack
    substrings s = [take n (drop i s) | i <- [0 .. length s], n <- [1 .. length s - i]]
