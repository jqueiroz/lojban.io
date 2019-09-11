#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/../.."

# Change directory to assets
cd assets

# Install node packages
npm install

# Compile first-party scripts
./node_modules/.bin/gulp scripts

# Compile third-party scripts
./node_modules/.bin/gulp vendors:scripts
