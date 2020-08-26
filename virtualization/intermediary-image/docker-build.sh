#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/../.."

# Build docker image
docker build -t lojbanios-dependencies -f ./virtualization/intermediary-image/Dockerfile .
