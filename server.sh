#! /usr/bin/env bash
set -euo pipefail

# The serve build compiles the /prijzen Elm calculator via `elm make`, so `elm`
# must be on PATH. shell.nix provides it; run this from inside the dev shell.
if ! command -v elm >/dev/null 2>&1; then
  echo "server.sh: 'elm' not found on PATH." >&2
  echo "Run inside the dev shell, e.g.: nix-shell --run ./server.sh" >&2
  exit 1
fi

nix-build shake && ./result/bin/shake-blog serve
