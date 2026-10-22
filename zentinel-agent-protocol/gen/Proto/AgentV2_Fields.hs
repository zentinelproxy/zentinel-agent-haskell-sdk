{- This file was auto-generated from agent_v2.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.AgentV2_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
accepted ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "accepted" a) =>
  Lens.Family2.LensLike' f s a
accepted = Data.ProtoLens.Field.field @"accepted"
action ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "action" a) =>
  Lens.Family2.LensLike' f s a
action = Data.ProtoLens.Field.field @"action"
activeThreads ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "activeThreads" a) =>
  Lens.Family2.LensLike' f s a
activeThreads = Data.ProtoLens.Field.field @"activeThreads"
add ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "add" a) =>
  Lens.Family2.LensLike' f s a
add = Data.ProtoLens.Field.field @"add"
agentId ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "agentId" a) =>
  Lens.Family2.LensLike' f s a
agentId = Data.ProtoLens.Field.field @"agentId"
allow ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "allow" a) =>
  Lens.Family2.LensLike' f s a
allow = Data.ProtoLens.Field.field @"allow"
audit ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "audit" a) =>
  Lens.Family2.LensLike' f s a
audit = Data.ProtoLens.Field.field @"audit"
avgLatencyMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "avgLatencyMs" a) =>
  Lens.Family2.LensLike' f s a
avgLatencyMs = Data.ProtoLens.Field.field @"avgLatencyMs"
block ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "block" a) =>
  Lens.Family2.LensLike' f s a
block = Data.ProtoLens.Field.field @"block"
blockingAgentId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "blockingAgentId" a) =>
  Lens.Family2.LensLike' f s a
blockingAgentId = Data.ProtoLens.Field.field @"blockingAgentId"
body ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "body" a) =>
  Lens.Family2.LensLike' f s a
body = Data.ProtoLens.Field.field @"body"
buckets ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "buckets" a) =>
  Lens.Family2.LensLike' f s a
buckets = Data.ProtoLens.Field.field @"buckets"
bufferAvailable ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "bufferAvailable" a) =>
  Lens.Family2.LensLike' f s a
bufferAvailable = Data.ProtoLens.Field.field @"bufferAvailable"
bytesReceived ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "bytesReceived" a) =>
  Lens.Family2.LensLike' f s a
bytesReceived = Data.ProtoLens.Field.field @"bytesReceived"
bytesSent ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "bytesSent" a) =>
  Lens.Family2.LensLike' f s a
bytesSent = Data.ProtoLens.Field.field @"bytesSent"
bytesTransferred ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "bytesTransferred" a) =>
  Lens.Family2.LensLike' f s a
bytesTransferred = Data.ProtoLens.Field.field @"bytesTransferred"
cancel ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "cancel" a) =>
  Lens.Family2.LensLike' f s a
cancel = Data.ProtoLens.Field.field @"cancel"
cancellation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "cancellation" a) =>
  Lens.Family2.LensLike' f s a
cancellation = Data.ProtoLens.Field.field @"cancellation"
capabilities ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "capabilities" a) =>
  Lens.Family2.LensLike' f s a
capabilities = Data.ProtoLens.Field.field @"capabilities"
challenge ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "challenge" a) =>
  Lens.Family2.LensLike' f s a
challenge = Data.ProtoLens.Field.field @"challenge"
challengeType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "challengeType" a) =>
  Lens.Family2.LensLike' f s a
challengeType = Data.ProtoLens.Field.field @"challengeType"
chunkIndex ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "chunkIndex" a) =>
  Lens.Family2.LensLike' f s a
chunkIndex = Data.ProtoLens.Field.field @"chunkIndex"
clientIp ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "clientIp" a) =>
  Lens.Family2.LensLike' f s a
clientIp = Data.ProtoLens.Field.field @"clientIp"
clientPort ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "clientPort" a) =>
  Lens.Family2.LensLike' f s a
clientPort = Data.ProtoLens.Field.field @"clientPort"
clientToServer ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "clientToServer" a) =>
  Lens.Family2.LensLike' f s a
clientToServer = Data.ProtoLens.Field.field @"clientToServer"
concurrentRequests ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "concurrentRequests" a) =>
  Lens.Family2.LensLike' f s a
concurrentRequests
  = Data.ProtoLens.Field.field @"concurrentRequests"
confidence ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "confidence" a) =>
  Lens.Family2.LensLike' f s a
confidence = Data.ProtoLens.Field.field @"confidence"
configError ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "configError" a) =>
  Lens.Family2.LensLike' f s a
configError = Data.ProtoLens.Field.field @"configError"
configJson ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "configJson" a) =>
  Lens.Family2.LensLike' f s a
configJson = Data.ProtoLens.Field.field @"configJson"
configPush ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "configPush" a) =>
  Lens.Family2.LensLike' f s a
configPush = Data.ProtoLens.Field.field @"configPush"
configResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "configResponse" a) =>
  Lens.Family2.LensLike' f s a
configResponse = Data.ProtoLens.Field.field @"configResponse"
configUpdate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "configUpdate" a) =>
  Lens.Family2.LensLike' f s a
configUpdate = Data.ProtoLens.Field.field @"configUpdate"
configVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "configVersion" a) =>
  Lens.Family2.LensLike' f s a
configVersion = Data.ProtoLens.Field.field @"configVersion"
configure ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "configure" a) =>
  Lens.Family2.LensLike' f s a
configure = Data.ProtoLens.Field.field @"configure"
connections ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "connections" a) =>
  Lens.Family2.LensLike' f s a
connections = Data.ProtoLens.Field.field @"connections"
content ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "content" a) =>
  Lens.Family2.LensLike' f s a
content = Data.ProtoLens.Field.field @"content"
contentType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "contentType" a) =>
  Lens.Family2.LensLike' f s a
contentType = Data.ProtoLens.Field.field @"contentType"
contextJson ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "contextJson" a) =>
  Lens.Family2.LensLike' f s a
contextJson = Data.ProtoLens.Field.field @"contextJson"
correlationId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "correlationId" a) =>
  Lens.Family2.LensLike' f s a
correlationId = Data.ProtoLens.Field.field @"correlationId"
count ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "count" a) =>
  Lens.Family2.LensLike' f s a
count = Data.ProtoLens.Field.field @"count"
counters ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "counters" a) =>
  Lens.Family2.LensLike' f s a
counters = Data.ProtoLens.Field.field @"counters"
cpuPercent ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "cpuPercent" a) =>
  Lens.Family2.LensLike' f s a
cpuPercent = Data.ProtoLens.Field.field @"cpuPercent"
custom ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "custom" a) =>
  Lens.Family2.LensLike' f s a
custom = Data.ProtoLens.Field.field @"custom"
data' ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "data'" a) =>
  Lens.Family2.LensLike' f s a
data' = Data.ProtoLens.Field.field @"data'"
definitionJson ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "definitionJson" a) =>
  Lens.Family2.LensLike' f s a
definitionJson = Data.ProtoLens.Field.field @"definitionJson"
description ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "description" a) =>
  Lens.Family2.LensLike' f s a
description = Data.ProtoLens.Field.field @"description"
disabledFeatures ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "disabledFeatures" a) =>
  Lens.Family2.LensLike' f s a
disabledFeatures = Data.ProtoLens.Field.field @"disabledFeatures"
drain ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "drain" a) =>
  Lens.Family2.LensLike' f s a
drain = Data.ProtoLens.Field.field @"drain"
drainEtaMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "drainEtaMs" a) =>
  Lens.Family2.LensLike' f s a
drainEtaMs = Data.ProtoLens.Field.field @"drainEtaMs"
durationMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "durationMs" a) =>
  Lens.Family2.LensLike' f s a
durationMs = Data.ProtoLens.Field.field @"durationMs"
enabled ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "enabled" a) =>
  Lens.Family2.LensLike' f s a
enabled = Data.ProtoLens.Field.field @"enabled"
error ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "error" a) =>
  Lens.Family2.LensLike' f s a
error = Data.ProtoLens.Field.field @"error"
fdLimit ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "fdLimit" a) =>
  Lens.Family2.LensLike' f s a
fdLimit = Data.ProtoLens.Field.field @"fdLimit"
features ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "features" a) =>
  Lens.Family2.LensLike' f s a
features = Data.ProtoLens.Field.field @"features"
field ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "field" a) =>
  Lens.Family2.LensLike' f s a
field = Data.ProtoLens.Field.field @"field"
fields ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "fields" a) =>
  Lens.Family2.LensLike' f s a
fields = Data.ProtoLens.Field.field @"fields"
flowControl ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "flowControl" a) =>
  Lens.Family2.LensLike' f s a
flowControl = Data.ProtoLens.Field.field @"flowControl"
frameType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "frameType" a) =>
  Lens.Family2.LensLike' f s a
frameType = Data.ProtoLens.Field.field @"frameType"
fromCache ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fromCache" a) =>
  Lens.Family2.LensLike' f s a
fromCache = Data.ProtoLens.Field.field @"fromCache"
gauges ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "gauges" a) =>
  Lens.Family2.LensLike' f s a
gauges = Data.ProtoLens.Field.field @"gauges"
gracePeriodMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "gracePeriodMs" a) =>
  Lens.Family2.LensLike' f s a
gracePeriodMs = Data.ProtoLens.Field.field @"gracePeriodMs"
guardrail ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "guardrail" a) =>
  Lens.Family2.LensLike' f s a
guardrail = Data.ProtoLens.Field.field @"guardrail"
guardrails ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "guardrails" a) =>
  Lens.Family2.LensLike' f s a
guardrails = Data.ProtoLens.Field.field @"guardrails"
handshake ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "handshake" a) =>
  Lens.Family2.LensLike' f s a
handshake = Data.ProtoLens.Field.field @"handshake"
headers ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "headers" a) =>
  Lens.Family2.LensLike' f s a
headers = Data.ProtoLens.Field.field @"headers"
health ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "health" a) =>
  Lens.Family2.LensLike' f s a
health = Data.ProtoLens.Field.field @"health"
healthConfig ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "healthConfig" a) =>
  Lens.Family2.LensLike' f s a
healthConfig = Data.ProtoLens.Field.field @"healthConfig"
healthReporting ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "healthReporting" a) =>
  Lens.Family2.LensLike' f s a
healthReporting = Data.ProtoLens.Field.field @"healthReporting"
help ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "help" a) =>
  Lens.Family2.LensLike' f s a
help = Data.ProtoLens.Field.field @"help"
histograms ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "histograms" a) =>
  Lens.Family2.LensLike' f s a
histograms = Data.ProtoLens.Field.field @"histograms"
httpVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "httpVersion" a) =>
  Lens.Family2.LensLike' f s a
httpVersion = Data.ProtoLens.Field.field @"httpVersion"
id ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "id" a) =>
  Lens.Family2.LensLike' f s a
id = Data.ProtoLens.Field.field @"id"
inFlight ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "inFlight" a) =>
  Lens.Family2.LensLike' f s a
inFlight = Data.ProtoLens.Field.field @"inFlight"
includeLoadMetrics ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "includeLoadMetrics" a) =>
  Lens.Family2.LensLike' f s a
includeLoadMetrics
  = Data.ProtoLens.Field.field @"includeLoadMetrics"
includeResourceMetrics ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "includeResourceMetrics" a) =>
  Lens.Family2.LensLike' f s a
includeResourceMetrics
  = Data.ProtoLens.Field.field @"includeResourceMetrics"
intervalMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "intervalMs" a) =>
  Lens.Family2.LensLike' f s a
intervalMs = Data.ProtoLens.Field.field @"intervalMs"
isInitial ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "isInitial" a) =>
  Lens.Family2.LensLike' f s a
isInitial = Data.ProtoLens.Field.field @"isInitial"
isLast ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "isLast" a) =>
  Lens.Family2.LensLike' f s a
isLast = Data.ProtoLens.Field.field @"isLast"
key ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "key" a) =>
  Lens.Family2.LensLike' f s a
key = Data.ProtoLens.Field.field @"key"
labels ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "labels" a) =>
  Lens.Family2.LensLike' f s a
labels = Data.ProtoLens.Field.field @"labels"
le ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "le" a) =>
  Lens.Family2.LensLike' f s a
le = Data.ProtoLens.Field.field @"le"
level ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "level" a) =>
  Lens.Family2.LensLike' f s a
level = Data.ProtoLens.Field.field @"level"
limits ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "limits" a) =>
  Lens.Family2.LensLike' f s a
limits = Data.ProtoLens.Field.field @"limits"
listId ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "listId" a) =>
  Lens.Family2.LensLike' f s a
listId = Data.ProtoLens.Field.field @"listId"
listUpdate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "listUpdate" a) =>
  Lens.Family2.LensLike' f s a
listUpdate = Data.ProtoLens.Field.field @"listUpdate"
load ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "load" a) =>
  Lens.Family2.LensLike' f s a
load = Data.ProtoLens.Field.field @"load"
log ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "log" a) =>
  Lens.Family2.LensLike' f s a
log = Data.ProtoLens.Field.field @"log"
manualReason ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "manualReason" a) =>
  Lens.Family2.LensLike' f s a
manualReason = Data.ProtoLens.Field.field @"manualReason"
maxBodySize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxBodySize" a) =>
  Lens.Family2.LensLike' f s a
maxBodySize = Data.ProtoLens.Field.field @"maxBodySize"
maxConcurrency ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxConcurrency" a) =>
  Lens.Family2.LensLike' f s a
maxConcurrency = Data.ProtoLens.Field.field @"maxConcurrency"
maxMemory ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxMemory" a) =>
  Lens.Family2.LensLike' f s a
maxMemory = Data.ProtoLens.Field.field @"maxMemory"
maxProcessingTimeMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxProcessingTimeMs" a) =>
  Lens.Family2.LensLike' f s a
maxProcessingTimeMs
  = Data.ProtoLens.Field.field @"maxProcessingTimeMs"
maybe'activeThreads ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'activeThreads" a) =>
  Lens.Family2.LensLike' f s a
maybe'activeThreads
  = Data.ProtoLens.Field.field @"maybe'activeThreads"
maybe'add ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'add" a) =>
  Lens.Family2.LensLike' f s a
maybe'add = Data.ProtoLens.Field.field @"maybe'add"
maybe'allow ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'allow" a) =>
  Lens.Family2.LensLike' f s a
maybe'allow = Data.ProtoLens.Field.field @"maybe'allow"
maybe'audit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'audit" a) =>
  Lens.Family2.LensLike' f s a
maybe'audit = Data.ProtoLens.Field.field @"maybe'audit"
maybe'block ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'block" a) =>
  Lens.Family2.LensLike' f s a
maybe'block = Data.ProtoLens.Field.field @"maybe'block"
maybe'blockingAgentId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'blockingAgentId" a) =>
  Lens.Family2.LensLike' f s a
maybe'blockingAgentId
  = Data.ProtoLens.Field.field @"maybe'blockingAgentId"
maybe'body ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'body" a) =>
  Lens.Family2.LensLike' f s a
maybe'body = Data.ProtoLens.Field.field @"maybe'body"
maybe'bufferAvailable ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'bufferAvailable" a) =>
  Lens.Family2.LensLike' f s a
maybe'bufferAvailable
  = Data.ProtoLens.Field.field @"maybe'bufferAvailable"
maybe'cancel ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'cancel" a) =>
  Lens.Family2.LensLike' f s a
maybe'cancel = Data.ProtoLens.Field.field @"maybe'cancel"
maybe'capabilities ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'capabilities" a) =>
  Lens.Family2.LensLike' f s a
maybe'capabilities
  = Data.ProtoLens.Field.field @"maybe'capabilities"
maybe'challenge ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'challenge" a) =>
  Lens.Family2.LensLike' f s a
maybe'challenge = Data.ProtoLens.Field.field @"maybe'challenge"
maybe'confidence ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'confidence" a) =>
  Lens.Family2.LensLike' f s a
maybe'confidence = Data.ProtoLens.Field.field @"maybe'confidence"
maybe'configError ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'configError" a) =>
  Lens.Family2.LensLike' f s a
maybe'configError = Data.ProtoLens.Field.field @"maybe'configError"
maybe'configResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'configResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'configResponse
  = Data.ProtoLens.Field.field @"maybe'configResponse"
maybe'configUpdate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'configUpdate" a) =>
  Lens.Family2.LensLike' f s a
maybe'configUpdate
  = Data.ProtoLens.Field.field @"maybe'configUpdate"
maybe'configVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'configVersion" a) =>
  Lens.Family2.LensLike' f s a
maybe'configVersion
  = Data.ProtoLens.Field.field @"maybe'configVersion"
maybe'configure ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'configure" a) =>
  Lens.Family2.LensLike' f s a
maybe'configure = Data.ProtoLens.Field.field @"maybe'configure"
maybe'connections ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'connections" a) =>
  Lens.Family2.LensLike' f s a
maybe'connections = Data.ProtoLens.Field.field @"maybe'connections"
maybe'correlationId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'correlationId" a) =>
  Lens.Family2.LensLike' f s a
maybe'correlationId
  = Data.ProtoLens.Field.field @"maybe'correlationId"
maybe'cpuPercent ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'cpuPercent" a) =>
  Lens.Family2.LensLike' f s a
maybe'cpuPercent = Data.ProtoLens.Field.field @"maybe'cpuPercent"
maybe'decision ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'decision" a) =>
  Lens.Family2.LensLike' f s a
maybe'decision = Data.ProtoLens.Field.field @"maybe'decision"
maybe'description ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'description" a) =>
  Lens.Family2.LensLike' f s a
maybe'description = Data.ProtoLens.Field.field @"maybe'description"
maybe'drain ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'drain" a) =>
  Lens.Family2.LensLike' f s a
maybe'drain = Data.ProtoLens.Field.field @"maybe'drain"
maybe'drainEtaMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'drainEtaMs" a) =>
  Lens.Family2.LensLike' f s a
maybe'drainEtaMs = Data.ProtoLens.Field.field @"maybe'drainEtaMs"
maybe'error ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'error" a) =>
  Lens.Family2.LensLike' f s a
maybe'error = Data.ProtoLens.Field.field @"maybe'error"
maybe'fdLimit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'fdLimit" a) =>
  Lens.Family2.LensLike' f s a
maybe'fdLimit = Data.ProtoLens.Field.field @"maybe'fdLimit"
maybe'features ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'features" a) =>
  Lens.Family2.LensLike' f s a
maybe'features = Data.ProtoLens.Field.field @"maybe'features"
maybe'field ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'field" a) =>
  Lens.Family2.LensLike' f s a
maybe'field = Data.ProtoLens.Field.field @"maybe'field"
maybe'flowControl ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'flowControl" a) =>
  Lens.Family2.LensLike' f s a
maybe'flowControl = Data.ProtoLens.Field.field @"maybe'flowControl"
maybe'guardrail ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'guardrail" a) =>
  Lens.Family2.LensLike' f s a
maybe'guardrail = Data.ProtoLens.Field.field @"maybe'guardrail"
maybe'handshake ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'handshake" a) =>
  Lens.Family2.LensLike' f s a
maybe'handshake = Data.ProtoLens.Field.field @"maybe'handshake"
maybe'health ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'health" a) =>
  Lens.Family2.LensLike' f s a
maybe'health = Data.ProtoLens.Field.field @"maybe'health"
maybe'healthConfig ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'healthConfig" a) =>
  Lens.Family2.LensLike' f s a
maybe'healthConfig
  = Data.ProtoLens.Field.field @"maybe'healthConfig"
maybe'help ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'help" a) =>
  Lens.Family2.LensLike' f s a
maybe'help = Data.ProtoLens.Field.field @"maybe'help"
maybe'limits ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'limits" a) =>
  Lens.Family2.LensLike' f s a
maybe'limits = Data.ProtoLens.Field.field @"maybe'limits"
maybe'listUpdate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'listUpdate" a) =>
  Lens.Family2.LensLike' f s a
maybe'listUpdate = Data.ProtoLens.Field.field @"maybe'listUpdate"
maybe'load ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'load" a) =>
  Lens.Family2.LensLike' f s a
maybe'load = Data.ProtoLens.Field.field @"maybe'load"
maybe'log ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'log" a) =>
  Lens.Family2.LensLike' f s a
maybe'log = Data.ProtoLens.Field.field @"maybe'log"
maybe'manualReason ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'manualReason" a) =>
  Lens.Family2.LensLike' f s a
maybe'manualReason
  = Data.ProtoLens.Field.field @"maybe'manualReason"
maybe'maxMemory ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'maxMemory" a) =>
  Lens.Family2.LensLike' f s a
maybe'maxMemory = Data.ProtoLens.Field.field @"maybe'maxMemory"
maybe'maxProcessingTimeMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'maxProcessingTimeMs" a) =>
  Lens.Family2.LensLike' f s a
maybe'maxProcessingTimeMs
  = Data.ProtoLens.Field.field @"maybe'maxProcessingTimeMs"
maybe'memoryBytes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'memoryBytes" a) =>
  Lens.Family2.LensLike' f s a
maybe'memoryBytes = Data.ProtoLens.Field.field @"maybe'memoryBytes"
maybe'memoryLimit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'memoryLimit" a) =>
  Lens.Family2.LensLike' f s a
maybe'memoryLimit = Data.ProtoLens.Field.field @"maybe'memoryLimit"
maybe'message ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'message" a) =>
  Lens.Family2.LensLike' f s a
maybe'message = Data.ProtoLens.Field.field @"maybe'message"
maybe'metadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'metadata" a) =>
  Lens.Family2.LensLike' f s a
maybe'metadata = Data.ProtoLens.Field.field @"maybe'metadata"
maybe'metrics ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'metrics" a) =>
  Lens.Family2.LensLike' f s a
maybe'metrics = Data.ProtoLens.Field.field @"maybe'metrics"
maybe'model ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'model" a) =>
  Lens.Family2.LensLike' f s a
maybe'model = Data.ProtoLens.Field.field @"maybe'model"
maybe'openFds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'openFds" a) =>
  Lens.Family2.LensLike' f s a
maybe'openFds = Data.ProtoLens.Field.field @"maybe'openFds"
maybe'operation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'operation" a) =>
  Lens.Family2.LensLike' f s a
maybe'operation = Data.ProtoLens.Field.field @"maybe'operation"
maybe'ping ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'ping" a) =>
  Lens.Family2.LensLike' f s a
maybe'ping = Data.ProtoLens.Field.field @"maybe'ping"
maybe'pong ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'pong" a) =>
  Lens.Family2.LensLike' f s a
maybe'pong = Data.ProtoLens.Field.field @"maybe'pong"
maybe'processingTimeMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'processingTimeMs" a) =>
  Lens.Family2.LensLike' f s a
maybe'processingTimeMs
  = Data.ProtoLens.Field.field @"maybe'processingTimeMs"
maybe'redirect ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'redirect" a) =>
  Lens.Family2.LensLike' f s a
maybe'redirect = Data.ProtoLens.Field.field @"maybe'redirect"
maybe'remove ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'remove" a) =>
  Lens.Family2.LensLike' f s a
maybe'remove = Data.ProtoLens.Field.field @"maybe'remove"
maybe'requestBodyChunk ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'requestBodyChunk" a) =>
  Lens.Family2.LensLike' f s a
maybe'requestBodyChunk
  = Data.ProtoLens.Field.field @"maybe'requestBodyChunk"
maybe'requestComplete ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'requestComplete" a) =>
  Lens.Family2.LensLike' f s a
maybe'requestComplete
  = Data.ProtoLens.Field.field @"maybe'requestComplete"
maybe'requestHeaders ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'requestHeaders" a) =>
  Lens.Family2.LensLike' f s a
maybe'requestHeaders
  = Data.ProtoLens.Field.field @"maybe'requestHeaders"
maybe'requestReload ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'requestReload" a) =>
  Lens.Family2.LensLike' f s a
maybe'requestReload
  = Data.ProtoLens.Field.field @"maybe'requestReload"
maybe'resources ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'resources" a) =>
  Lens.Family2.LensLike' f s a
maybe'resources = Data.ProtoLens.Field.field @"maybe'resources"
maybe'response ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'response" a) =>
  Lens.Family2.LensLike' f s a
maybe'response = Data.ProtoLens.Field.field @"maybe'response"
maybe'responseBodyChunk ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'responseBodyChunk" a) =>
  Lens.Family2.LensLike' f s a
maybe'responseBodyChunk
  = Data.ProtoLens.Field.field @"maybe'responseBodyChunk"
maybe'responseHeaders ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'responseHeaders" a) =>
  Lens.Family2.LensLike' f s a
maybe'responseHeaders
  = Data.ProtoLens.Field.field @"maybe'responseHeaders"
maybe'restartRequired ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'restartRequired" a) =>
  Lens.Family2.LensLike' f s a
maybe'restartRequired
  = Data.ProtoLens.Field.field @"maybe'restartRequired"
maybe'routeId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'routeId" a) =>
  Lens.Family2.LensLike' f s a
maybe'routeId = Data.ProtoLens.Field.field @"maybe'routeId"
maybe'ruleUpdate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'ruleUpdate" a) =>
  Lens.Family2.LensLike' f s a
maybe'ruleUpdate = Data.ProtoLens.Field.field @"maybe'ruleUpdate"
maybe'serverName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'serverName" a) =>
  Lens.Family2.LensLike' f s a
maybe'serverName = Data.ProtoLens.Field.field @"maybe'serverName"
maybe'set ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'set" a) =>
  Lens.Family2.LensLike' f s a
maybe'set = Data.ProtoLens.Field.field @"maybe'set"
maybe'shutdown ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'shutdown" a) =>
  Lens.Family2.LensLike' f s a
maybe'shutdown = Data.ProtoLens.Field.field @"maybe'shutdown"
maybe'tlsVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'tlsVersion" a) =>
  Lens.Family2.LensLike' f s a
maybe'tlsVersion = Data.ProtoLens.Field.field @"maybe'tlsVersion"
maybe'totalSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'totalSize" a) =>
  Lens.Family2.LensLike' f s a
maybe'totalSize = Data.ProtoLens.Field.field @"maybe'totalSize"
maybe'traceparent ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'traceparent" a) =>
  Lens.Family2.LensLike' f s a
maybe'traceparent = Data.ProtoLens.Field.field @"maybe'traceparent"
maybe'unhealthyReason ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'unhealthyReason" a) =>
  Lens.Family2.LensLike' f s a
maybe'unhealthyReason
  = Data.ProtoLens.Field.field @"maybe'unhealthyReason"
maybe'updateType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'updateType" a) =>
  Lens.Family2.LensLike' f s a
maybe'updateType = Data.ProtoLens.Field.field @"maybe'updateType"
maybe'upstream ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'upstream" a) =>
  Lens.Family2.LensLike' f s a
maybe'upstream = Data.ProtoLens.Field.field @"maybe'upstream"
maybe'upstreamId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'upstreamId" a) =>
  Lens.Family2.LensLike' f s a
maybe'upstreamId = Data.ProtoLens.Field.field @"maybe'upstreamId"
maybe'validUntilMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'validUntilMs" a) =>
  Lens.Family2.LensLike' f s a
maybe'validUntilMs
  = Data.ProtoLens.Field.field @"maybe'validUntilMs"
maybe'websocketFrame ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'websocketFrame" a) =>
  Lens.Family2.LensLike' f s a
maybe'websocketFrame
  = Data.ProtoLens.Field.field @"maybe'websocketFrame"
memoryBytes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "memoryBytes" a) =>
  Lens.Family2.LensLike' f s a
memoryBytes = Data.ProtoLens.Field.field @"memoryBytes"
memoryLimit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "memoryLimit" a) =>
  Lens.Family2.LensLike' f s a
memoryLimit = Data.ProtoLens.Field.field @"memoryLimit"
message ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "message" a) =>
  Lens.Family2.LensLike' f s a
message = Data.ProtoLens.Field.field @"message"
metadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "metadata" a) =>
  Lens.Family2.LensLike' f s a
metadata = Data.ProtoLens.Field.field @"metadata"
method ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "method" a) =>
  Lens.Family2.LensLike' f s a
method = Data.ProtoLens.Field.field @"method"
metrics ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "metrics" a) =>
  Lens.Family2.LensLike' f s a
metrics = Data.ProtoLens.Field.field @"metrics"
metricsExport ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "metricsExport" a) =>
  Lens.Family2.LensLike' f s a
metricsExport = Data.ProtoLens.Field.field @"metricsExport"
model ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "model" a) =>
  Lens.Family2.LensLike' f s a
model = Data.ProtoLens.Field.field @"model"
name ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "name" a) =>
  Lens.Family2.LensLike' f s a
name = Data.ProtoLens.Field.field @"name"
needsMore ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "needsMore" a) =>
  Lens.Family2.LensLike' f s a
needsMore = Data.ProtoLens.Field.field @"needsMore"
openFds ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "openFds" a) =>
  Lens.Family2.LensLike' f s a
openFds = Data.ProtoLens.Field.field @"openFds"
p50LatencyMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "p50LatencyMs" a) =>
  Lens.Family2.LensLike' f s a
p50LatencyMs = Data.ProtoLens.Field.field @"p50LatencyMs"
p95LatencyMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "p95LatencyMs" a) =>
  Lens.Family2.LensLike' f s a
p95LatencyMs = Data.ProtoLens.Field.field @"p95LatencyMs"
p99LatencyMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "p99LatencyMs" a) =>
  Lens.Family2.LensLike' f s a
p99LatencyMs = Data.ProtoLens.Field.field @"p99LatencyMs"
params ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "params" a) =>
  Lens.Family2.LensLike' f s a
params = Data.ProtoLens.Field.field @"params"
payload ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "payload" a) =>
  Lens.Family2.LensLike' f s a
payload = Data.ProtoLens.Field.field @"payload"
ping ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ping" a) =>
  Lens.Family2.LensLike' f s a
ping = Data.ProtoLens.Field.field @"ping"
pingTimestampMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "pingTimestampMs" a) =>
  Lens.Family2.LensLike' f s a
pingTimestampMs = Data.ProtoLens.Field.field @"pingTimestampMs"
pong ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "pong" a) =>
  Lens.Family2.LensLike' f s a
pong = Data.ProtoLens.Field.field @"pong"
preferredChunkSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "preferredChunkSize" a) =>
  Lens.Family2.LensLike' f s a
preferredChunkSize
  = Data.ProtoLens.Field.field @"preferredChunkSize"
priority ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "priority" a) =>
  Lens.Family2.LensLike' f s a
priority = Data.ProtoLens.Field.field @"priority"
processingTimeMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "processingTimeMs" a) =>
  Lens.Family2.LensLike' f s a
processingTimeMs = Data.ProtoLens.Field.field @"processingTimeMs"
protocol ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "protocol" a) =>
  Lens.Family2.LensLike' f s a
protocol = Data.ProtoLens.Field.field @"protocol"
protocolVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "protocolVersion" a) =>
  Lens.Family2.LensLike' f s a
protocolVersion = Data.ProtoLens.Field.field @"protocolVersion"
proxyBufferAvailable ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "proxyBufferAvailable" a) =>
  Lens.Family2.LensLike' f s a
proxyBufferAvailable
  = Data.ProtoLens.Field.field @"proxyBufferAvailable"
proxyId ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "proxyId" a) =>
  Lens.Family2.LensLike' f s a
proxyId = Data.ProtoLens.Field.field @"proxyId"
proxyVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "proxyVersion" a) =>
  Lens.Family2.LensLike' f s a
proxyVersion = Data.ProtoLens.Field.field @"proxyVersion"
queueDepth ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "queueDepth" a) =>
  Lens.Family2.LensLike' f s a
queueDepth = Data.ProtoLens.Field.field @"queueDepth"
reason ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "reason" a) =>
  Lens.Family2.LensLike' f s a
reason = Data.ProtoLens.Field.field @"reason"
reasonCodes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "reasonCodes" a) =>
  Lens.Family2.LensLike' f s a
reasonCodes = Data.ProtoLens.Field.field @"reasonCodes"
recoverable ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "recoverable" a) =>
  Lens.Family2.LensLike' f s a
recoverable = Data.ProtoLens.Field.field @"recoverable"
redirect ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "redirect" a) =>
  Lens.Family2.LensLike' f s a
redirect = Data.ProtoLens.Field.field @"redirect"
remove ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "remove" a) =>
  Lens.Family2.LensLike' f s a
remove = Data.ProtoLens.Field.field @"remove"
removeRules ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "removeRules" a) =>
  Lens.Family2.LensLike' f s a
removeRules = Data.ProtoLens.Field.field @"removeRules"
reportIntervalMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "reportIntervalMs" a) =>
  Lens.Family2.LensLike' f s a
reportIntervalMs = Data.ProtoLens.Field.field @"reportIntervalMs"
requestBodyChunk ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "requestBodyChunk" a) =>
  Lens.Family2.LensLike' f s a
requestBodyChunk = Data.ProtoLens.Field.field @"requestBodyChunk"
requestComplete ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "requestComplete" a) =>
  Lens.Family2.LensLike' f s a
requestComplete = Data.ProtoLens.Field.field @"requestComplete"
requestHeaders ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "requestHeaders" a) =>
  Lens.Family2.LensLike' f s a
requestHeaders = Data.ProtoLens.Field.field @"requestHeaders"
requestId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "requestId" a) =>
  Lens.Family2.LensLike' f s a
requestId = Data.ProtoLens.Field.field @"requestId"
requestReload ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "requestReload" a) =>
  Lens.Family2.LensLike' f s a
requestReload = Data.ProtoLens.Field.field @"requestReload"
requestsProcessed ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "requestsProcessed" a) =>
  Lens.Family2.LensLike' f s a
requestsProcessed = Data.ProtoLens.Field.field @"requestsProcessed"
requestsRejected ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "requestsRejected" a) =>
  Lens.Family2.LensLike' f s a
requestsRejected = Data.ProtoLens.Field.field @"requestsRejected"
requestsTimedOut ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "requestsTimedOut" a) =>
  Lens.Family2.LensLike' f s a
requestsTimedOut = Data.ProtoLens.Field.field @"requestsTimedOut"
resources ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "resources" a) =>
  Lens.Family2.LensLike' f s a
resources = Data.ProtoLens.Field.field @"resources"
response ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "response" a) =>
  Lens.Family2.LensLike' f s a
response = Data.ProtoLens.Field.field @"response"
responseBodyChunk ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "responseBodyChunk" a) =>
  Lens.Family2.LensLike' f s a
responseBodyChunk = Data.ProtoLens.Field.field @"responseBodyChunk"
responseHeaders ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "responseHeaders" a) =>
  Lens.Family2.LensLike' f s a
responseHeaders = Data.ProtoLens.Field.field @"responseHeaders"
restartRequired ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "restartRequired" a) =>
  Lens.Family2.LensLike' f s a
restartRequired = Data.ProtoLens.Field.field @"restartRequired"
routeId ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "routeId" a) =>
  Lens.Family2.LensLike' f s a
routeId = Data.ProtoLens.Field.field @"routeId"
ruleIds ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ruleIds" a) =>
  Lens.Family2.LensLike' f s a
ruleIds = Data.ProtoLens.Field.field @"ruleIds"
ruleSet ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ruleSet" a) =>
  Lens.Family2.LensLike' f s a
ruleSet = Data.ProtoLens.Field.field @"ruleSet"
ruleUpdate ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "ruleUpdate" a) =>
  Lens.Family2.LensLike' f s a
ruleUpdate = Data.ProtoLens.Field.field @"ruleUpdate"
rules ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "rules" a) =>
  Lens.Family2.LensLike' f s a
rules = Data.ProtoLens.Field.field @"rules"
sequence ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "sequence" a) =>
  Lens.Family2.LensLike' f s a
sequence = Data.ProtoLens.Field.field @"sequence"
serverName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "serverName" a) =>
  Lens.Family2.LensLike' f s a
serverName = Data.ProtoLens.Field.field @"serverName"
set ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "set" a) =>
  Lens.Family2.LensLike' f s a
set = Data.ProtoLens.Field.field @"set"
shutdown ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "shutdown" a) =>
  Lens.Family2.LensLike' f s a
shutdown = Data.ProtoLens.Field.field @"shutdown"
state ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "state" a) =>
  Lens.Family2.LensLike' f s a
state = Data.ProtoLens.Field.field @"state"
status ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "status" a) =>
  Lens.Family2.LensLike' f s a
status = Data.ProtoLens.Field.field @"status"
statusCode ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "statusCode" a) =>
  Lens.Family2.LensLike' f s a
statusCode = Data.ProtoLens.Field.field @"statusCode"
streamingBody ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "streamingBody" a) =>
  Lens.Family2.LensLike' f s a
streamingBody = Data.ProtoLens.Field.field @"streamingBody"
success ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "success" a) =>
  Lens.Family2.LensLike' f s a
success = Data.ProtoLens.Field.field @"success"
sum ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "sum" a) =>
  Lens.Family2.LensLike' f s a
sum = Data.ProtoLens.Field.field @"sum"
supportedEvents ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "supportedEvents" a) =>
  Lens.Family2.LensLike' f s a
supportedEvents = Data.ProtoLens.Field.field @"supportedEvents"
supportedVersions ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "supportedVersions" a) =>
  Lens.Family2.LensLike' f s a
supportedVersions = Data.ProtoLens.Field.field @"supportedVersions"
tags ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "tags" a) =>
  Lens.Family2.LensLike' f s a
tags = Data.ProtoLens.Field.field @"tags"
timeoutMultiplier ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "timeoutMultiplier" a) =>
  Lens.Family2.LensLike' f s a
timeoutMultiplier = Data.ProtoLens.Field.field @"timeoutMultiplier"
timestampMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "timestampMs" a) =>
  Lens.Family2.LensLike' f s a
timestampMs = Data.ProtoLens.Field.field @"timestampMs"
tlsVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "tlsVersion" a) =>
  Lens.Family2.LensLike' f s a
tlsVersion = Data.ProtoLens.Field.field @"tlsVersion"
totalSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "totalSize" a) =>
  Lens.Family2.LensLike' f s a
totalSize = Data.ProtoLens.Field.field @"totalSize"
traceparent ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "traceparent" a) =>
  Lens.Family2.LensLike' f s a
traceparent = Data.ProtoLens.Field.field @"traceparent"
unhealthyReason ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "unhealthyReason" a) =>
  Lens.Family2.LensLike' f s a
unhealthyReason = Data.ProtoLens.Field.field @"unhealthyReason"
upstream ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "upstream" a) =>
  Lens.Family2.LensLike' f s a
upstream = Data.ProtoLens.Field.field @"upstream"
upstreamId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "upstreamId" a) =>
  Lens.Family2.LensLike' f s a
upstreamId = Data.ProtoLens.Field.field @"upstreamId"
uri ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "uri" a) =>
  Lens.Family2.LensLike' f s a
uri = Data.ProtoLens.Field.field @"uri"
url ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "url" a) =>
  Lens.Family2.LensLike' f s a
url = Data.ProtoLens.Field.field @"url"
validUntilMs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "validUntilMs" a) =>
  Lens.Family2.LensLike' f s a
validUntilMs = Data.ProtoLens.Field.field @"validUntilMs"
value ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "value" a) =>
  Lens.Family2.LensLike' f s a
value = Data.ProtoLens.Field.field @"value"
vec'add ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "vec'add" a) =>
  Lens.Family2.LensLike' f s a
vec'add = Data.ProtoLens.Field.field @"vec'add"
vec'buckets ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'buckets" a) =>
  Lens.Family2.LensLike' f s a
vec'buckets = Data.ProtoLens.Field.field @"vec'buckets"
vec'counters ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'counters" a) =>
  Lens.Family2.LensLike' f s a
vec'counters = Data.ProtoLens.Field.field @"vec'counters"
vec'disabledFeatures ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'disabledFeatures" a) =>
  Lens.Family2.LensLike' f s a
vec'disabledFeatures
  = Data.ProtoLens.Field.field @"vec'disabledFeatures"
vec'gauges ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'gauges" a) =>
  Lens.Family2.LensLike' f s a
vec'gauges = Data.ProtoLens.Field.field @"vec'gauges"
vec'headers ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'headers" a) =>
  Lens.Family2.LensLike' f s a
vec'headers = Data.ProtoLens.Field.field @"vec'headers"
vec'histograms ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'histograms" a) =>
  Lens.Family2.LensLike' f s a
vec'histograms = Data.ProtoLens.Field.field @"vec'histograms"
vec'reasonCodes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'reasonCodes" a) =>
  Lens.Family2.LensLike' f s a
vec'reasonCodes = Data.ProtoLens.Field.field @"vec'reasonCodes"
vec'remove ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'remove" a) =>
  Lens.Family2.LensLike' f s a
vec'remove = Data.ProtoLens.Field.field @"vec'remove"
vec'removeRules ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'removeRules" a) =>
  Lens.Family2.LensLike' f s a
vec'removeRules = Data.ProtoLens.Field.field @"vec'removeRules"
vec'requestHeaders ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'requestHeaders" a) =>
  Lens.Family2.LensLike' f s a
vec'requestHeaders
  = Data.ProtoLens.Field.field @"vec'requestHeaders"
vec'responseHeaders ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'responseHeaders" a) =>
  Lens.Family2.LensLike' f s a
vec'responseHeaders
  = Data.ProtoLens.Field.field @"vec'responseHeaders"
vec'ruleIds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'ruleIds" a) =>
  Lens.Family2.LensLike' f s a
vec'ruleIds = Data.ProtoLens.Field.field @"vec'ruleIds"
vec'rules ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'rules" a) =>
  Lens.Family2.LensLike' f s a
vec'rules = Data.ProtoLens.Field.field @"vec'rules"
vec'supportedEvents ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'supportedEvents" a) =>
  Lens.Family2.LensLike' f s a
vec'supportedEvents
  = Data.ProtoLens.Field.field @"vec'supportedEvents"
vec'supportedVersions ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'supportedVersions" a) =>
  Lens.Family2.LensLike' f s a
vec'supportedVersions
  = Data.ProtoLens.Field.field @"vec'supportedVersions"
vec'tags ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'tags" a) =>
  Lens.Family2.LensLike' f s a
vec'tags = Data.ProtoLens.Field.field @"vec'tags"
version ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "version" a) =>
  Lens.Family2.LensLike' f s a
version = Data.ProtoLens.Field.field @"version"
websocket ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "websocket" a) =>
  Lens.Family2.LensLike' f s a
websocket = Data.ProtoLens.Field.field @"websocket"
websocketFrame ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "websocketFrame" a) =>
  Lens.Family2.LensLike' f s a
websocketFrame = Data.ProtoLens.Field.field @"websocketFrame"