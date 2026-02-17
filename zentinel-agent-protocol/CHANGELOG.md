# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0.0] - 2026-01-17

### Added

- Initial release with full Protocol v2 support
- Core types for all protocol messages (events, decisions, responses)
- `AgentHandler` typeclass for implementing agent logic
- Unix Domain Socket (UDS) transport with length-prefixed framing
- gRPC transport (interface only, full implementation pending)
- Health reporting utilities (`healthyStatus`, `degradedStatus`, etc.)
- Metrics collection utilities (`MetricsCollector`, standard metric names)
- Server runner with multi-transport support
- Example agents: `echo-agent` and `waf-agent`

### Design Decisions

- Uses GHC2021 language standard (requires GHC 9.8+)
- Effect system: `ReaderT env IO` with `unliftio` for safe async operations
- JSON serialization via `aeson` for wire protocol compatibility
- Strict fields by default for predictable memory usage

[Unreleased]: https://github.com/zentinelproxy/zentinel-agent-haskell/compare/v0.1.0.0...HEAD
[0.1.0.0]: https://github.com/zentinelproxy/zentinel-agent-haskell/releases/tag/v0.1.0.0
