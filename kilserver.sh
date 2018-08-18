#! /run/current-system/sw/bin/bash
set -xe

ps -aux > /tmp/procs
proc=$(cat /tmp/procs | grep pelican | sed "s/$USER *//" | sed "s/ .*$//")
kill $proc
