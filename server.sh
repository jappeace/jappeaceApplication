#! /usr/bin/env bash

nix-build shake && ./result/bin/shake-blog serve
