<p align="center">
  <a href="https://zentinelproxy.io">
    <picture>
      <source media="(prefers-color-scheme: dark)" srcset="https://raw.githubusercontent.com/zentinelproxy/design/main/marks/lockup/zentinel-lockup-dark.svg">
      <img alt="Zentinel" src="https://raw.githubusercontent.com/zentinelproxy/design/main/marks/lockup/zentinel-lockup-light.svg" height="52">
    </picture>
  </a>
</p>

<h3 align="center">Haskell agent SDK</h3>

<p align="center">Write Zentinel agents in Haskell, speaking agent protocol v2 over a Unix socket.</p>

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
helpers. On paper its Unix socket transport matches the v2 framing and
handshake (untested, see below); the gRPC module is a placeholder that uses its own framing, not gRPC. It is not on
Hackage.

> **Status (checked on 2026-10-21 against Zentinel 0.6.50): the package does not build.** Its dependency bounds
> cannot be satisfied. It asks for `grpc-haskell >= 0.3`, but Hackage has
> nothing above 0.1; and with GHC 9.8, which `cabal.project` selects, its
> `containers >= 0.7` bound conflicts with the boot packages. Nothing here,
> including the quick start, has been run against Zentinel. There is no CI.

## Quick start

Create `Main.hs` in an executable that depends on `zentinel-agent-protocol`
(see [Install](#install)):

```haskell
module Main where

import Zentinel.Agent.Protocol

instance AgentHandler IO where
  capabilities = pure (defaultCapabilities "hello")
  onRequestHeaders _ =
    pure allow {respRequestHeaders = [HeaderSet "X-Hello" "from-agent"]}

main :: IO ()
main = runAgent defaultConfig {scSocketPath = Just "/tmp/hello.sock"} id
```

Run it with `cabal run`. It would listen on `/tmp/hello.sock`.

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

With this SDK as it is today, that 503 is what you get: see the status note
above.

## Install

Not on Hackage. Once the bounds are fixed, depend on it from Git in your
`cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/zentinelproxy/zentinel-agent-haskell-sdk
  subdir: zentinel-agent-protocol
```

and add `zentinel-agent-protocol` to your executable's `build-depends`.

## API overview

- **`AgentHandler m`**: `capabilities` (required), `onRequestHeaders`
  (required), `onRequestBodyChunk`, `onResponseHeaders`, `onResponseBodyChunk`,
  `onWebSocketFrame`, `onRequestComplete`, `healthStatus`, `metricsReport`,
  `onConfigure`, `onShutdown`, `onDrain`, `onCancel`.
- **`runAgent config runner`**: serves the handler; `ServerConfig` has
  `scSocketPath`, `scGrpcAddress`, `scLogLevel`, and `defaultConfig` uses
  `/tmp/zentinel-agent.sock`.
- **Responses**: `allow`, `block status body`, `redirect`, `challenge`, and
  record update on `AgentResponse` (`respRequestHeaders`,
  `respResponseHeaders` with `HeaderSet` / `HeaderAdd` / `HeaderRemove`).
- **Capabilities**: `defaultCapabilities name`, `defaultFeatures`,
  `defaultLimits`; health and metrics builders in `Zentinel.Agent.Health` and
  `Zentinel.Agent.Metrics`.

Examples: [`examples/echo-agent`](examples/echo-agent/Main.hs) and
[`examples/waf-agent`](examples/waf-agent/Main.hs).

## Development

```bash
cabal build all
cabal test
```

`cabal.project` selects GHC 9.8 and builds the library and both examples; the
package declares `tested-with: GHC == 9.8.2, GHC == 9.10.1`. Both commands
currently stop in the dependency solver, as described above.

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
