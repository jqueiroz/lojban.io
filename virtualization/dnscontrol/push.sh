#!/usr/bin/env bash
set -e

nix-shell --pure shell.nix --run "dnscontrol push --creds !creds.py"
