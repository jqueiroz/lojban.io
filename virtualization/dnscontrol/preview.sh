#!/usr/bin/env bash
set -e

nix-shell --pure shell.nix --run "dnscontrol preview --creds !creds.py"
