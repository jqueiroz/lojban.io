#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/.."

# Start new containers
./dist/docker.sh run -p 8080:8000/tcp -it --rm --name lojban-dev-server lojban-dev-server
