-- |
-- The conformance agent of the Zentinel SDK conformance harness
-- (@sdk-conformance/README.md@ in zentinelproxy/zentinel):
--
-- @
-- conformance-agent --socket /tmp/conformance.sock
-- conformance-agent --grpc 127.0.0.1:50051
-- @
module Main where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Zentinel.Agent.Protocol

data Env = Env
  { envConfig :: IORef (Maybe Value)
  , envBodies :: IORef (M.Map Text BS.ByteString)
  }

instance AgentHandler (ReaderT Env IO) where
  capabilities =
    pure
      (defaultCapabilities "conformance")
        { capName = "Conformance agent"
        , capSupportedEvents = [RequestHeaders, RequestBodyChunk, ResponseHeaders]
        }

  onConfigure config _version = do
    env <- ask
    liftIO (writeIORef (envConfig env) (Just config))
    pure True

  onRequestHeaders event
    | "/block" `T.isPrefixOf` reqHdrUri event = pure (block 403 "blocked-by-agent")
    | otherwise = do
        env <- ask
        value <- liftIO (configValue <$> readIORef (envConfig env))
        pure
          ( setRequestHeader "X-Conformance-Config" value $
              setRequestHeader "X-Hello" "from-agent" allow
          )

  onRequestBodyChunk event = do
    env <- ask
    let cid = reqBodyCorrelationId event
        chunk = either (const BS.empty) id (B64.decode (TE.encodeUtf8 (reqBodyData event)))
    body <- liftIO $ atomicModifyIORef' (envBodies env) $ \m ->
      let b = M.findWithDefault BS.empty cid m <> chunk
       in if reqBodyIsLast event then (M.delete cid m, b) else (M.insert cid b m, b)
    if not (reqBodyIsLast event)
      then pure allow {respNeedsMore = True}
      else do
        let text = TE.decodeUtf8With TE.lenientDecode body
        pure $
          if "block-this-body" `T.isInfixOf` text
            then block 403 ("body-seen:" <> text)
            else allow

  onResponseHeaders _ = pure (setResponseHeader "X-Conformance-Response" "from-agent" allow)

-- | @config.nested.value@, or @unconfigured@.
configValue :: Maybe Value -> Text
configValue (Just (Object o))
  | Just (Object nested) <- KM.lookup "nested" o
  , Just (String v) <- KM.lookup "value" nested =
      v
configValue _ = "unconfigured"

main :: IO ()
main = do
  env <- Env <$> newIORef Nothing <*> newIORef M.empty
  config <- configFromArgs defaultConfig
  runAgent config (`runReaderT` env)
