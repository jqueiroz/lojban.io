#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/.."

# Function to run commands inside a Nix shell
run_command(){
    nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.03.tar.gz "./buildscripts/environments/haskell.nix" --run "$1" --keep LOJBAN_TOOL_ENVIRONMENT --keep LOJBAN_TOOL_OAUTH2_GOOGLE_CLIENT_ID --keep LOJBAN_TOOL_OAUTH2_GOOGLE_CLIENT_SECRET --keep LOJBAN_TOOL_REDIS_HOSTNAME
}

COMMAND="stack --no-nix-pure $*"

if [[ -n "$LOJBAN_TOOL_BYPASS_NIX" ]]; then
    echo "--> Runing: $COMMAND"
    $COMMAND
else
    # Setup nix-shell environment
    echo "--> Step 1 - Preparing nix-shell environment..."
    run_command ""

    # Run stack command
    echo "--> Step 2 - Runing: $COMMAND"
    run_command "$COMMAND"
fi
