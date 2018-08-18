#! /run/current-system/sw/bin/bash
while true; do
mkdir -p output
(cd output; python -m pelican.server)
done
