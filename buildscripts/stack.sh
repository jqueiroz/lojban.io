#!/usr/bin/env bash
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

run_command(){
    nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.03.tar.gz "$DIR/haskell-environment.nix" --run "$1"
}

# Setup nix-shell environment
echo "*** Step 1 - Preparing nix-shell environment..."
run_command ""

# Run stack command
echo "*** Step 2 - Runing: stack $*"
run_command "stack $*"
