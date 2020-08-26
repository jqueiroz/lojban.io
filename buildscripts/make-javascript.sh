#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/.."

# Function to run commands inside a Nix shell
run_command(){
    nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.03.tar.gz --pure "./buildscripts/environments/javascript.nix" --run "$1"
}

if [[ -n "$LOJBANIOS_BYPASS_NIX" ]]; then
    echo "--> Generating javascript files..."
    ./buildscripts/helpers/make-javascript.sh
else
    # Setup nix-shell environment
    echo "--> Step 1 - Preparing nix-shell environment..."
    run_command ""

    # Compile .ts files
    echo "--> Step 2 - Generating javascript files..."
    run_command "./buildscripts/helpers/make-javascript.sh"
fi
