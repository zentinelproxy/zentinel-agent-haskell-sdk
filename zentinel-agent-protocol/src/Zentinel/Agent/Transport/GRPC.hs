{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Zentinel.Agent.Transport.GRPC
-- Description : Agent protocol v2 over gRPC
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : experimental
--
-- Serves @zentinel.agent.v2.AgentServiceV2@ (the proxy's
-- @agent_v2.proto@, vendored under @proto/@) with
-- <https://hackage.haskell.org/package/grapesy grapesy>.
--
-- The proxy opens one bidirectional @ProcessStream@ per connection. It waits
-- for the stream's response headers before it sends anything, so they are
-- sent as soon as the stream opens. Then it sends a handshake, a
-- @ConfigureEvent@ followed by a @Ping@ (the @Pong@ tells it the
-- configuration was applied; a rejected configuration ends the stream), and
-- events. Events are handled concurrently; every response carries its
-- request's correlation id.
module Zentinel.Agent.Transport.GRPC
  ( GrpcServerConfig (..)
  , runGrpcServer
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (Value (..), eitherDecodeStrict, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as B64
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as M
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Vector qualified as V
import Data.Int (Int32)
import Data.Word (Word16, Word32, Word64)
import Network.GRPC.Common (NoMetadata (..), StreamElem (..))
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf (Proto (..), Protobuf, defMessage, getProto, (&), (.~), (^.))
import Network.GRPC.Server qualified as G
import Network.GRPC.Server.Run qualified as G
import Proto.AgentV2 qualified as P
import Zentinel.Agent.Handler
import Zentinel.Agent.Internal.Logging
import Zentinel.Agent.Transport.UDS (configure)
import Zentinel.Agent.Types

type ProcessStream = Protobuf P.AgentServiceV2 "processStream"

type instance G.RequestMetadata (Protobuf P.AgentServiceV2 meth) = NoMetadata
type instance G.ResponseInitialMetadata (Protobuf P.AgentServiceV2 meth) = NoMetadata
type instance G.ResponseTrailingMetadata (Protobuf P.AgentServiceV2 meth) = NoMetadata

-- | gRPC server configuration
data GrpcServerConfig = GrpcServerConfig
  { grpcHost :: !Text
  , grpcPort :: !Word16
  , grpcLogAction :: !(LogAction IO LogMessage)
  }

-- | Serve the agent over gRPC (plaintext HTTP/2) until the thread is killed.
runGrpcServer ::
  forall m.
  AgentHandler m =>
  GrpcServerConfig ->
  (forall a. m a -> IO a) ->
  IO ()
runGrpcServer config runM = do
  let logger = grpcLogAction config
  logInfo logger $
    "gRPC server listening on " <> grpcHost config <> ":" <> T.pack (show (grpcPort config))
  G.runServerWithHandlers
    G.def
    G.ServerConfig
      { G.serverInsecure =
          Just
            G.InsecureConfig
              { G.insecureHost = Just (T.unpack (grpcHost config))
              , G.insecurePort = fromIntegral (grpcPort config)
              }
      , G.serverSecure = Nothing
      }
    [G.someRpcHandler (G.mkRpcHandler @ProcessStream (processStream logger runM))]

processStream ::
  forall m.
  AgentHandler m =>
  LogAction IO LogMessage ->
  (forall a. m a -> IO a) ->
  G.Call ProcessStream ->
  IO ()
processStream logger runM call = do
  -- The proxy waits for the response headers before it sends the handshake.
  G.initiateResponse call
  sendLock <- newMVar ()
  inFlight <- newTVarIO (0 :: Int)
  let send :: P.AgentToProxy -> IO ()
      send msg = withMVar sendLock $ \_ -> G.sendNextOutput call (Proto msg)
      respond cid act = do
        atomically (modifyTVar' inFlight (+ 1))
        void . forkIO $ do
          resp <-
            try (runM act) >>= \case
              Right r -> pure r
              Left (e :: SomeException) -> do
                logError logger $ "Handler failed: " <> T.pack (show e)
                pure allow
          send (defMessage & #response .~ responseToProto cid resp)
          atomically (modifyTVar' inFlight (subtract 1))
      finish = do
        atomically (readTVar inFlight >>= check . (== 0))
        G.sendTrailers call NoMetadata
      handle :: P.ProxyToAgent -> IO Bool
      handle msg = case msg ^. #maybe'message of
        Just (P.ProxyToAgent'Handshake hs) -> do
          caps <- runM capabilities
          send (defMessage & #handshake .~ handshakeToProto caps (hs ^. #supportedVersions))
          pure (protocolVersion `elem` (hs ^. #supportedVersions))
        Just (P.ProxyToAgent'Configure ev) -> do
          let cfg = either (const Null) id (eitherDecodeStrict (TE.encodeUtf8 (ev ^. #configJson)))
          -- In order: the Ping that follows is answered only once this is applied.
          resp <- runM (configure cfg (ev ^. #maybe'configVersion))
          case respDecision resp of
            Allow -> pure True
            _ -> do
              logError logger "Configuration rejected; ending the stream"
              pure False
        Just (P.ProxyToAgent'Ping ping) -> do
          now <- nowMs
          send
            ( defMessage
                & #pong
                  .~ ( defMessage
                        & #sequence .~ (ping ^. #sequence)
                        & #pingTimestampMs .~ (ping ^. #timestampMs)
                        & #timestampMs .~ now
                     )
            )
          pure True
        Just (P.ProxyToAgent'RequestHeaders ev) -> do
          let e = requestHeadersFromProto ev
          respond (metaCorrelationId (reqHdrMetadata e)) (onRequestHeaders e)
          pure True
        Just (P.ProxyToAgent'RequestBodyChunk ev) -> do
          let e = requestBodyFromProto ev
          respond (reqBodyCorrelationId e) (onRequestBodyChunk e)
          pure True
        Just (P.ProxyToAgent'ResponseHeaders ev) -> do
          let e = responseHeadersFromProto ev
          respond (respHdrCorrelationId e) (onResponseHeaders e)
          pure True
        Just (P.ProxyToAgent'ResponseBodyChunk ev) -> do
          let e = responseBodyFromProto ev
          respond (respBodyCorrelationId e) (onResponseBodyChunk e)
          pure True
        Just (P.ProxyToAgent'RequestComplete ev) -> do
          let e = requestCompleteFromProto ev
          respond (reqCompCorrelationId e) (onRequestComplete e >> pure allow)
          pure True
        Just (P.ProxyToAgent'WebsocketFrame ev) -> do
          let e = webSocketFromProto ev
          respond (wsFrameCorrelationId e) (onWebSocketFrame e)
          pure True
        Just (P.ProxyToAgent'Guardrail ev) -> do
          let e = guardrailFromProto ev
          respond (grInspectCorrelationId e) (guardrailVerdict <$> onGuardrailInspect e)
          pure True
        Just (P.ProxyToAgent'Cancel _) -> pure True
        Nothing -> pure True
      loop =
        G.recvInput call >>= \case
          StreamElem msg -> handle (getProto msg) >>= \continue -> if continue then loop else finish
          FinalElem msg _ -> handle (getProto msg) >> finish
          NoMoreElems _ -> finish
  loop

nowMs :: IO Word64
nowMs = round . (* 1000) <$> getPOSIXTime

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

handshakeToProto :: AgentCapabilities -> [Word32] -> P.HandshakeResponse
handshakeToProto caps versions =
  defMessage
    & #protocolVersion .~ protocolVersion
    & #success .~ ok
    & #maybe'error .~ (if ok then Nothing else Just "protocol v2 not offered by the proxy")
    & #capabilities
      .~ ( defMessage
            & #protocolVersion .~ protocolVersion
            & #agentId .~ capAgentId caps
            & #name .~ capName caps
            & #version .~ capVersion caps
            & #supportedEvents .~ map grpcEventNumber (capSupportedEvents caps)
            & #features
              .~ ( defMessage
                    & #streamingBody .~ featStreamingBody f
                    & #websocket .~ featWebsocket f
                    & #guardrails .~ featGuardrails f
                    & #configPush .~ featConfigPush f
                    & #metricsExport .~ featMetricsExport f
                    & #concurrentRequests .~ featConcurrentRequests f
                    & #cancellation .~ featCancellation f
                    & #flowControl .~ featFlowControl f
                    & #healthReporting .~ featHealthReporting f
                 )
            & #limits
              .~ ( defMessage
                    & #maxBodySize .~ fromIntegral (limMaxBodySize l)
                    & #maxConcurrency .~ limMaxConcurrency l
                    & #preferredChunkSize .~ fromIntegral (limPreferredChunkSize l)
                 )
            & #healthConfig
              .~ (defMessage & #reportIntervalMs .~ healthReportIntervalMs (capHealthConfig caps))
         )
  where
    ok = protocolVersion `elem` versions
    f = capFeatures caps
    l = capLimits caps

-- | The number of an event type in the gRPC @EventType@ enum.
grpcEventNumber :: EventType -> Int32
grpcEventNumber = \case
  RequestHeaders -> 1
  RequestBodyChunk -> 2
  ResponseHeaders -> 3
  ResponseBodyChunk -> 4
  RequestComplete -> 5
  WebSocketFrame -> 6
  GuardrailInspect -> 7
  Configure -> 8

headersFromProto :: [P.Header] -> HM.HashMap Text (V.Vector Text)
headersFromProto hs =
  HM.map V.fromList (HM.fromListWith (flip (<>)) [(h ^. #name, [h ^. #value]) | h <- hs])

requestHeadersFromProto :: P.RequestHeadersEvent -> RequestHeadersEvent
requestHeadersFromProto ev =
  RequestHeadersEvent
    { reqHdrMetadata =
        RequestMetadata
          { metaCorrelationId = md ^. #correlationId
          , metaRequestId = md ^. #requestId
          , metaClientIp = md ^. #clientIp
          , metaClientPort = fromIntegral (md ^. #clientPort)
          , metaServerName = md ^. #maybe'serverName
          , metaProtocol = md ^. #protocol
          , metaTlsVersion = md ^. #maybe'tlsVersion
          , metaTlsCipher = Nothing
          , metaRouteId = md ^. #maybe'routeId
          , metaUpstreamId = md ^. #maybe'upstreamId
          , metaTimestamp = T.pack (show (md ^. #timestampMs))
          , metaTraceparent = md ^. #maybe'traceparent
          }
    , reqHdrMethod = ev ^. #method
    , reqHdrUri = ev ^. #uri
    , reqHdrHeaders = headersFromProto (ev ^. #headers)
    }
  where
    md = ev ^. #metadata

b64 :: ByteString -> Text
b64 = TE.decodeUtf8 . B64.encode

requestBodyFromProto :: P.BodyChunkEvent -> RequestBodyChunkEvent
requestBodyFromProto ev =
  RequestBodyChunkEvent
    { reqBodyCorrelationId = ev ^. #correlationId
    , reqBodyData = b64 (ev ^. #data')
    , reqBodyIsLast = ev ^. #isLast
    , reqBodyTotalSize = fromIntegral <$> ev ^. #maybe'totalSize
    , reqBodyChunkIndex = ev ^. #chunkIndex
    , reqBodyBytesReceived = fromIntegral (ev ^. #bytesTransferred)
    }

responseBodyFromProto :: P.BodyChunkEvent -> ResponseBodyChunkEvent
responseBodyFromProto ev =
  ResponseBodyChunkEvent
    { respBodyCorrelationId = ev ^. #correlationId
    , respBodyData = b64 (ev ^. #data')
    , respBodyIsLast = ev ^. #isLast
    , respBodyTotalSize = fromIntegral <$> ev ^. #maybe'totalSize
    , respBodyChunkIndex = ev ^. #chunkIndex
    , respBodyBytesSent = fromIntegral (ev ^. #bytesTransferred)
    }

responseHeadersFromProto :: P.ResponseHeadersEvent -> ResponseHeadersEvent
responseHeadersFromProto ev =
  ResponseHeadersEvent
    { respHdrCorrelationId = ev ^. #correlationId
    , respHdrStatus = fromIntegral (ev ^. #statusCode)
    , respHdrHeaders = headersFromProto (ev ^. #headers)
    }

requestCompleteFromProto :: P.RequestCompleteEvent -> RequestCompleteEvent
requestCompleteFromProto ev =
  RequestCompleteEvent
    { reqCompCorrelationId = ev ^. #correlationId
    , reqCompStatus = fromIntegral (ev ^. #statusCode)
    , reqCompDurationMs = ev ^. #durationMs
    , reqCompRequestBodySize = fromIntegral (ev ^. #bytesReceived)
    , reqCompResponseBodySize = fromIntegral (ev ^. #bytesSent)
    , reqCompUpstreamAttempts = 0
    , reqCompError = ev ^. #maybe'error
    }

webSocketFromProto :: P.WebSocketFrameEvent -> WebSocketFrameEvent
webSocketFromProto ev =
  WebSocketFrameEvent
    { wsFrameCorrelationId = ev ^. #correlationId
    , wsFrameOpcode = case ev ^. #frameType of
        2 -> WsBinary
        3 -> WsPing
        4 -> WsPong
        5 -> WsCloseFrame
        _ -> WsText
    , wsFrameData = b64 (ev ^. #payload)
    , wsFrameClientToServer = ev ^. #clientToServer
    , wsFrameIndex = 0
    , wsFrameFin = True
    , wsFrameRouteId = Nothing
    , wsFrameClientIp = ""
    }

guardrailFromProto :: P.GuardrailInspectEvent -> GuardrailInspectEvent
guardrailFromProto ev =
  GuardrailInspectEvent
    { grInspectCorrelationId = ev ^. #correlationId
    , grInspectType = PromptInjection
    , grInspectContent = ev ^. #content
    , grInspectModel = ev ^. #maybe'model
    , grInspectCategories = []
    , grInspectRouteId = Nothing
    , grInspectMetadata = HM.empty
    }

-- | gRPC responses have no routing metadata, so a guardrail verdict travels
-- as the decision: a detection blocks.
guardrailVerdict :: GuardrailResponse -> AgentResponse
guardrailVerdict r
  | grRespDetected r =
      block 403 $ case grRespDetections r of
        d : _ -> detDescription d
        [] -> "Blocked by guardrail agent"
  | otherwise = allow

headerOpToProto :: HeaderOp -> P.HeaderOp
headerOpToProto = \case
  HeaderSet n v -> defMessage & #set .~ (defMessage & #name .~ n & #value .~ v)
  HeaderAdd n v -> defMessage & #add .~ (defMessage & #name .~ n & #value .~ v)
  HeaderRemove n -> defMessage & #remove .~ n

responseToProto :: Text -> AgentResponse -> P.AgentResponse
responseToProto cid r =
  withDecision (respDecision r) $
    defMessage
      & #correlationId .~ cid
      & #requestHeaders .~ map headerOpToProto (respRequestHeaders r)
      & #responseHeaders .~ map headerOpToProto (respResponseHeaders r)
      & #needsMore .~ respNeedsMore r
      & #audit
        .~ ( defMessage
              & #tags .~ auditTags a
              & #ruleIds .~ auditRuleIds a
              & #reasonCodes .~ auditReasonCodes a
              & #maybe'confidence .~ (realToFrac <$> auditConfidence a)
              & #custom .~ M.fromList [(k, customText v) | (k, v) <- HM.toList (auditCustom a)]
           )
  where
    a = respAudit r
    customText (String s) = s
    customText v = TE.decodeUtf8 (LBS.toStrict (encode v))
    withDecision d msg = case d of
      Allow -> msg & #allow .~ defMessage
      Block s body hdrs ->
        msg
          & #block
            .~ ( defMessage
                  & #status .~ fromIntegral s
                  & #maybe'body .~ body
                  & #headers
                    .~ [defMessage & #name .~ n & #value .~ v | (n, v) <- maybe [] HM.toList hdrs]
               )
      Redirect url s -> msg & #redirect .~ (defMessage & #url .~ url & #status .~ fromIntegral s)
      Challenge t ps ->
        msg
          & #challenge
            .~ (defMessage & #challengeType .~ t & #params .~ M.fromList (HM.toList ps))
