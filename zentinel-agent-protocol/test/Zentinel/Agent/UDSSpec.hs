{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Zentinel.Agent.UDSSpec
-- Description : The UDS server, spoken to over a real socket the way the proxy does
module Zentinel.Agent.UDSSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Data.Aeson (Value (..), decodeStrict, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Network.Socket
import System.Directory (doesPathExist, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Zentinel.Agent.Handler
import Zentinel.Agent.Transport.UDS
import Zentinel.Agent.Types
import Zentinel.Agent.Wire

-- | The configuration the test agent was given.
seenConfig :: IORef (Maybe Value)
seenConfig = unsafePerformIO (newIORef Nothing)
{-# NOINLINE seenConfig #-}

instance AgentHandler IO where
  capabilities = pure (defaultCapabilities "uds-test")
  onConfigure cfg _ = writeIORef seenConfig (Just cfg) >> pure (cfg /= String "reject")
  onRequestHeaders e
    | reqHdrUri e == "/slow" = threadDelay 300000 >> pure (setRequestHeader "X-Slow" "1" allow)
    | otherwise = pure (setRequestHeader "X-Hello" "from-agent" allow)

withServer :: (FilePath -> IO a) -> IO a
withServer act = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "zentinel-haskell-sdk-test.sock"
  bracket
    (forkIO (runUdsServer defaultUdsConfig {udsSocketPath = path} id))
    (\t -> killThread t >> cleanup path)
    (\_ -> waitFor path >> act path)
  where
    waitFor p = doesPathExist p >>= \e -> if e then pure () else threadDelay 10000 >> waitFor p
    cleanup p = doesPathExist p >>= \e -> if e then removeFile p else pure ()

connectTo :: FilePath -> IO Socket
connectTo path = do
  s <- socket AF_UNIX Stream defaultProtocol
  connect s (SockAddrUnix path)
  pure s

send :: Socket -> MessageType -> Value -> IO ()
send s t v = writeFrame s t (LBS.toStrict (encode v)) >>= either (fail . show) pure

sendRaw :: Socket -> MessageType -> ByteString -> IO ()
sendRaw s t b = writeFrame s t b >>= either (fail . show) pure

recv :: Socket -> IO (MessageType, ByteString)
recv s =
  readFrame s >>= \case
    Right (Just (b, p)) | Just t <- messageTypeFromByte b -> pure (t, p)
    other -> fail ("unexpected read: " <> show other)

recvJSON :: Socket -> IO (MessageType, Value)
recvJSON s = do
  (t, p) <- recv s
  maybe (fail "not JSON") (pure . (t,)) (decodeStrict p)

handshake :: Socket -> IO Value
handshake s = do
  send s MsgHandshakeRequest $
    object ["supported_versions" .= [2 :: Int], "proxy_id" .= ("test" :: String), "proxy_version" .= ("0" :: String), "config" .= Null]
  (t, v) <- recvJSON s
  t `shouldBe` MsgHandshakeResponse
  pure v

headersEvent :: String -> String -> Value
headersEvent cid uri =
  object
    [ "metadata" .= object ["correlation_id" .= cid, "request_id" .= cid, "client_ip" .= ("127.0.0.1" :: String), "client_port" .= (1 :: Int), "protocol" .= ("HTTP/1.1" :: String), "timestamp" .= ("t" :: String)]
    , "method" .= ("GET" :: String)
    , "uri" .= uri
    , "headers" .= object []
    ]

field :: [KM.Key] -> Value -> Value
field [] v = v
field (k : ks) (Object o) = maybe Null (field ks) (KM.lookup k o)
field _ _ = Null

spec :: Spec
spec = around withServer $ do
  it "answers the handshake with a v2 UdsHandshakeResponse" $ \path -> do
    s <- connectTo path
    reply <- handshake s
    field ["success"] reply `shouldBe` Bool True
    field ["protocol_version"] reply `shouldBe` Number 2
    field ["capabilities", "agent_id"] reply `shouldBe` String "uds-test-001"

  it "answers Configure with an allow carrying its correlation id, and applies it" $ \path -> do
    s <- connectTo path
    _ <- handshake s
    send s MsgConfigure $ object ["config" .= object ["k" .= ("v" :: String)], "config_version" .= Null, "correlation_id" .= ("cfg-1" :: String)]
    (t, v) <- recvJSON s
    t `shouldBe` MsgAgentResponse
    field ["decision"] v `shouldBe` String "allow"
    field ["audit", "custom", "correlation_id"] v `shouldBe` String "cfg-1"
    readIORef seenConfig `shouldReturn` Just (object ["k" .= ("v" :: String)])

  it "rejects a configuration the agent refuses with a block" $ \path -> do
    s <- connectTo path
    _ <- handshake s
    send s MsgConfigure $ object ["config" .= ("reject" :: String), "correlation_id" .= ("cfg-2" :: String)]
    (_, v) <- recvJSON s
    field ["decision", "block", "status"] v `shouldBe` Number 500

  it "answers a Ping with a Pong carrying the same payload" $ \path -> do
    s <- connectTo path
    _ <- handshake s
    sendRaw s MsgPing "{\"sequence\":7}"
    recv s `shouldReturn` (MsgPong, "{\"sequence\":7}")

  it "handles events concurrently, each response with its correlation id" $ \path -> do
    s <- connectTo path
    _ <- handshake s
    send s MsgRequestHeaders (headersEvent "slow" "/slow")
    send s MsgRequestHeaders (headersEvent "fast" "/fast")
    (_, first) <- recvJSON s
    (_, second) <- recvJSON s
    field ["audit", "custom", "correlation_id"] first `shouldBe` String "fast"
    field ["request_headers"] first `shouldBe` toJSONList [object ["set" .= object ["name" .= ("X-Hello" :: String), "value" .= ("from-agent" :: String)]]]
    field ["audit", "custom", "correlation_id"] second `shouldBe` String "slow"
  where
    toJSONList :: [Value] -> Value
    toJSONList xs = field ["x"] (object ["x" .= xs])
