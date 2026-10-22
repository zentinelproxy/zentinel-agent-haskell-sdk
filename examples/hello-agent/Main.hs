module Main where

import Zentinel.Agent.Protocol

instance AgentHandler IO where
  capabilities = pure (defaultCapabilities "hello")
  onRequestHeaders _ = pure (setRequestHeader "X-Hello" "from-agent" allow)

main :: IO ()
main = do
  config <- configFromArgs defaultConfig {scSocketPath = Just "/tmp/hello.sock"}
  runAgent config id
