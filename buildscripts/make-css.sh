#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/.."

# Function to run commands inside a Nix shell
run_command(){
    nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.03.tar.gz --pure "./buildscripts/env/css.nix" --run "$1"
}

if [[ -n "$LOJBAN_TOOL_BYPASS_NIX" ]]; then
    echo "--> Generating CSS files..."
    ./buildscripts/helpers/lessc.sh
else
    # Setup nix-shell environment
    echo "--> Step 1 - Preparing nix-shell environment..."
    run_command ""

    # Compile .less files
    echo "--> Step 2 - Generating CSS files..."
    run_command "./buildscripts/helpers/lessc.sh"
fi
