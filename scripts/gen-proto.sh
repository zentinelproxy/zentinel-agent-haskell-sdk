#!/usr/bin/env bash
# Regenerate zentinel-agent-protocol/gen/Proto/*.hs from proto/agent_v2.proto.
#
# The proto is a copy of crates/agent-protocol/proto/agent_v2.proto in
# zentinelproxy/zentinel. The generated modules are committed so that building
# the SDK needs neither protoc nor proto-lens-protoc. After updating the proto:
#
#   cabal install proto-lens-protoc --installdir="$HOME/.local/bin"
#   scripts/gen-proto.sh
set -euo pipefail
cd "$(dirname "$0")/../zentinel-agent-protocol"
plugin="${PROTO_LENS_PROTOC:-$(command -v proto-lens-protoc)}"
rm -rf gen/Proto
mkdir -p gen
protoc --plugin=protoc-gen-haskell="$plugin" --haskell_out=gen --proto_path=proto proto/agent_v2.proto
echo "regenerated: $(ls gen/Proto)"
