#!/usr/bin/env bash
# Run the Zentinel SDK conformance harness against this SDK's agents.
#
#   ZENTINEL_BIN=/path/to/zentinel \
#   ZENTINEL_SRC=/path/to/zentinelproxy/zentinel \
#   scripts/conformance.sh
#
# ZENTINEL_SRC is a checkout of zentinelproxy/zentinel (for
# sdk-conformance/run.py); ZENTINEL_BIN a zentinel binary, release 26.10_8 or
# later (the configure check needs the config { } delivery of zentinel#603,
# first shipped there). Without both, this skips. CI runs the same checks through the
# zentinelproxy/zentinel/sdk-conformance action.
set -euo pipefail
cd "$(dirname "$0")/.."
if [[ -z "${ZENTINEL_BIN:-}" || -z "${ZENTINEL_SRC:-}" ]]; then
  echo "skipping conformance: set ZENTINEL_BIN and ZENTINEL_SRC" >&2
  exit 0
fi
cabal build -v0 exe:conformance-agent exe:hello-agent
conformance="$(cabal list-bin conformance-agent)"
hello="$(cabal list-bin hello-agent)"
run="$ZENTINEL_SRC/sdk-conformance/run.py"
python3 "$run" --zentinel-bin "$ZENTINEL_BIN" \
  --agent-cmd "$conformance --socket {socket}" \
  --grpc-agent-cmd "$conformance --grpc {grpc}"
python3 "$run" --zentinel-bin "$ZENTINEL_BIN" --profile hello \
  --agent-cmd "$hello --socket {socket}"
