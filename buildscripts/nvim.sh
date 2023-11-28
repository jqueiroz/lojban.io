#!/usr/bin/env bash
set -e

# Identify the directory where this script is located
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Change directory to the project's root
cd "$DIR/.."

export NVIM_HLS_EXE="$DIR/hls/haskell-language-server-42"
nvim $*
