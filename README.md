<p align="center">
  <a href="https://zentinelproxy.io">
    <picture>
      <source media="(prefers-color-scheme: dark)" srcset="https://raw.githubusercontent.com/zentinelproxy/design/main/marks/lockup/zentinel-lockup-dark.svg">
      <img alt="Zentinel" src="https://raw.githubusercontent.com/zentinelproxy/design/main/marks/lockup/zentinel-lockup-light.svg" height="52">
    </picture>
  </a>
</p>

<h3 align="center">Haskell agent SDK</h3>

<p align="center">Write Zentinel agents in Haskell, speaking agent protocol v2 over a Unix socket or gRPC.</p>

<p align="center">
  <img alt="License: Apache-2.0" src="https://img.shields.io/badge/license-Apache--2.0-9b4d8a?style=flat&labelColor=1f161e">
  <img alt="Agent protocol: v2" src="https://img.shields.io/badge/agent%20protocol-v2-f68d31?style=flat&labelColor=1f161e">
</p>

<p align="center">
  <a href="https://docs.zentinelproxy.io/agents/v2/">Agent protocol</a> &middot;
  <a href="https://docs.zentinelproxy.io">Docs</a> &middot;
  <a href="https://github.com/zentinelproxy">All repositories</a>
</p>

The `zentinel-agent-protocol` package gives you an `AgentHandler` type class
with one method per event over any `MonadUnliftIO` monad (so state lives in a
`ReaderT`), the protocol types, response builders, and health and metrics
helpers. It serves agents over the v2 Unix socket transport and over gRPC
(the proxy's `zentinel.agent.v2.AgentServiceV2`, using
[grapesy](https://hackage.haskell.org/package/grapesy)). It is not on Hackage.

> **Status (checked on 2026-10-22 against Zentinel 26.10_8):** version 0.2.0.0
> passes the shared
> [SDK conformance harness](https://github.com/zentinelproxy/zentinel/tree/main/sdk-conformance)
> on both transports (handshake, configure, request and response headers,
> block, body inspection, reconnect), and the quick start below passes its
> `hello` profile. CI runs both on every push. The `configure` check needs
> Zentinel 26.10_8 or later, which sends an agent's `config { }` block on
> every connection (zentinel#603); releases up to 0.6.50 (26.10_7) never
> delivered it.
>
> Before 0.2.0.0 the package did not build (it asked for `grpc-haskell >= 0.3`,
> which does not exist, and `containers >= 0.7`, which conflicts with GHC 9.8's
> boot packages), its gRPC module was a placeholder with its own framing, and
> its Unix socket transport used v1 framing and JSON shapes the proxy rejects.

## Quick start

Create `Main.hs` in an executable that depends on `zentinel-agent-protocol`
(see [Install](#install)):

```haskell
module Main where

import Zentinel.Agent.Protocol

instance AgentHandler IO where
  capabilities = pure (defaultCapabilities "hello")
  onRequestHeaders _ = pure (setRequestHeader "X-Hello" "from-agent" allow)

main :: IO ()
main = do
  config <- configFromArgs defaultConfig {scSocketPath = Just "/tmp/hello.sock"}
  runAgent config id
```

This is [`examples/hello-agent`](examples/hello-agent/Main.hs); run it with
`cabal run hello-agent`. It listens on `/tmp/hello.sock` (`--socket PATH` to
change that, `--grpc HOST:PORT` to serve gRPC instead, both for both).

Then point Zentinel at the socket. This assumes your config already defines a
listener and an upstream named `backend`:

```kdl
agents {
    agent "hello" type="custom" {
        unix-socket "/tmp/hello.sock"
        events "request_headers"
        timeout-ms 100
    }
}

filters {
    filter "hello" {
        type "agent"
        agent "hello"
        failure-mode "closed"
    }
}

routes {
    route "default" {
        matches {
            path-prefix "/"
        }
        upstream "backend"
        filters "hello"
    }
}
```

Check the file with `zentinel test -c zentinel.kdl` and start Zentinel. Every
request on the route then reaches `backend` with `X-Hello: from-agent`.
`failure-mode "closed"` on the filter turns an unreachable agent into a 503
instead of letting requests through unprocessed, so a wiring mistake shows up
on the first request.

For gRPC, start the agent with `--grpc 127.0.0.1:50051` and use
`grpc "http://127.0.0.1:50051"` in place of `unix-socket`.

## Install

Not on Hackage. Depend on it from Git in your `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/zentinelproxy/zentinel-agent-haskell-sdk
  subdir: zentinel-agent-protocol
```

and add `zentinel-agent-protocol` to your executable's `build-depends`.

The gRPC transport (cabal flag `grpc`, on by default) depends on grapesy, which
links the C library snappy: `apt-get install libsnappy-dev` on Debian and
Ubuntu, `brew install snappy` on macOS (Homebrew's prefix is not on the default
search path, so add `extra-lib-dirs: /opt/homebrew/lib` and
`extra-include-dirs: /opt/homebrew/include` for `package snappy-c` in
`cabal.project.local`). For a Unix-socket-only build without it, add
`constraints: zentinel-agent-protocol -grpc`.

## API overview

- **`AgentHandler m`**: `capabilities` (required), `onRequestHeaders`
  (required), `onRequestBodyChunk`, `onResponseHeaders`, `onResponseBodyChunk`,
  `onWebSocketFrame`, `onRequestComplete`, `healthStatus`, `metricsReport`,
  `onConfigure`, `onShutdown`, `onDrain`, `onCancel`.
- **`runAgent config runner`**: serves the handler; `ServerConfig` has
  `scSocketPath`, `scGrpcAddress`, `scLogLevel`, and `defaultConfig` uses
  `/tmp/zentinel-agent.sock`. `configFromArgs` applies `--socket PATH`,
  `--grpc HOST:PORT` and `--debug`.
- **Configuration**: the proxy sends the agent's `config { }` block on every
  connection; it reaches `onConfigure` (return `False` to reject it, which
  keeps the proxy from using the connection).
- **Responses**: `allow`, `block status body`, `redirect`, `challenge`,
  `setRequestHeader` / `setResponseHeader`, and record update on
  `AgentResponse` (`respRequestHeaders`, `respResponseHeaders` with
  `HeaderSet` / `HeaderAdd` / `HeaderRemove`, `respNeedsMore` for body
  chunks).
- **Capabilities**: `defaultCapabilities name`, `defaultFeatures`,
  `defaultLimits`; health and metrics builders in `Zentinel.Agent.Health` and
  `Zentinel.Agent.Metrics`.

Examples: [`examples/hello-agent`](examples/hello-agent/Main.hs) (the quick
start), [`examples/conformance-agent`](examples/conformance-agent/Main.hs) (the
agent the conformance harness drives: config, headers, block, body
inspection), [`examples/echo-agent`](examples/echo-agent/Main.hs) and
[`examples/waf-agent`](examples/waf-agent/Main.hs).

## Development

```bash
cabal build all
cabal test all
```

The package declares `tested-with: GHC == 9.8.4`. The unit tests include a
Unix socket test that speaks the v2 frames to the server the way the proxy
does. To run the conformance harness locally against a zentinel from release
26.10_8 or later:

```bash
ZENTINEL_BIN=/path/to/zentinel/target/debug/zentinel \
ZENTINEL_SRC=/path/to/zentinel \
scripts/conformance.sh
```

`proto/agent_v2.proto` is a copy of the proxy's
`crates/agent-protocol/proto/agent_v2.proto`; the Haskell modules generated from
it are committed under `gen/` so the build needs no `protoc`. Regenerate them
with `scripts/gen-proto.sh` after updating the copy.

## Related

- [Agent protocol v2](https://docs.zentinelproxy.io/agents/v2/)
- [Agent configuration](https://docs.zentinelproxy.io/configuration/agents/)
- [Other SDKs](https://github.com/zentinelproxy?q=sdk)
- [Zentinel](https://github.com/zentinelproxy/zentinel)

## License

Apache-2.0, see [`LICENSE`](LICENSE).

---

<p align="center">
  <sub>Part of <a href="https://zentinelproxy.io">Zentinel</a>, the security-first reverse proxy. The shisa guards the gate.</sub>
</p>
