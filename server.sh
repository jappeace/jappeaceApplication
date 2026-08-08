#! /usr/bin/env bash
set -euo pipefail

# shake/default.nix wrapt het binary met elm op PATH en een UTF-8-locale,
# dus dit werkt zonder dev-shell: gewoon ./server.sh.
nix-build shake && ./result/bin/shake-blog serve
