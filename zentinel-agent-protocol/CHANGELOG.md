# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.0.0] - unreleased

Agent protocol v2 as the proxy speaks it (zentinel#600). Nothing in 0.1.0.0
could talk to Zentinel, and the package did not build.

### Fixed

- The package builds on GHC 9.8: dropped the nonexistent `grpc-haskell >= 0.3`
  and `proto3-*`/`http2` dependencies, and relaxed `containers` and other
  bounds that conflicted with GHC 9.8's boot packages.
- Unix socket transport: v2 framing (the length counts the type byte, then a
  type byte, then JSON), the `UdsHandshakeResponse` schema (every
  `capabilities` field, events as UDS numbers, `encoding`), the request's
  correlation id in `audit.custom.correlation_id` on every response, replies to
  Configure (0x17) before the next message is read, Ping/Pong, and concurrent
  event handling.
- JSON shapes the proxy parses: `Decision` and `WebSocketDecision` are
  externally tagged (`"allow"`, `{"block": {...}}`), `RequestCompleteEvent` and
  `WebSocketFrameEvent` use the proxy's field names, optional event fields
  default.
- The test suite ran zero tests (hspec-discover only finds `*Spec` modules) and
  did not compile; it now runs 42.
- `source-repository` points at zentinel-agent-haskell-sdk.

### Added

- A real gRPC transport (`Zentinel.Agent.Transport.GRPC`) serving
  `zentinel.agent.v2.AgentServiceV2` with grapesy, from the proxy's
  `agent_v2.proto` (cabal flag `grpc`, on by default; needs libsnappy).
- `Zentinel.Agent.Wire`: framing, message types and handshake payloads.
- `configFromArgs` / `parseArgs` (`--socket`, `--grpc`, `--debug`),
  `setRequestHeader`, `setResponseHeader`.
- `examples/hello-agent` (the README quick start) and
  `examples/conformance-agent`, run through the zentinel SDK conformance
  harness in CI.

### Removed

- The `AgentMessage` / `ProxyMessage` JSON envelopes, which were not part of
  the protocol, and the internal framing module they used.

## [0.1.0.0] - 2026-01-17

### Added

- Initial release with full Protocol v2 support
- Core types for all protocol messages (events, decisions, responses)
- `AgentHandler` typeclass for implementing agent logic
- Unix Domain Socket (UDS) transport with length-prefixed framing
- gRPC transport (interface only; it was not gRPC)
- Health reporting utilities (`healthyStatus`, `degradedStatus`, etc.)
- Metrics collection utilities (`MetricsCollector`, standard metric names)
- Server runner with multi-transport support
- Example agents: `echo-agent` and `waf-agent`

### Design Decisions

- Uses GHC2021 language standard (requires GHC 9.8+)
- Effect system: `ReaderT env IO` with `unliftio` for safe async operations
- JSON serialization via `aeson` for wire protocol compatibility
- Strict fields by default for predictable memory usage

[Unreleased]: https://github.com/zentinelproxy/zentinel-agent-haskell-sdk/compare/v0.1.0.0...HEAD
[0.1.0.0]: https://github.com/zentinelproxy/zentinel-agent-haskell-sdk/releases/tag/v0.1.0.0
