#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR"

# Function to run commands inside a Nix shell
run_command(){
    nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/nixos-23.11.tar.gz --pure "./buildscripts/environments/lint.nix" --run "$1"
}

if [[ -n "$LOJBANIOS_BYPASS_NIX" ]]; then
    ./buildscripts/helpers/lint.sh
else
    # Setup nix-shell environment
    echo "--> Step 1 - Preparing nix-shell environment..."
    run_command ""

    # Compile .less files
    echo "--> Step 2 - Running linters..."
    run_command "./buildscripts/helpers/lint.sh"
fi
