#!/usr/bin/env bash
set -e

# Change directory to the script's location
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

CONTAINER_NAME="$1"
if [ -z "$CONTAINER_NAME" ]; then
    echo "ERROR: missing container name"
    exit 1
fi

./docker.sh $DOCKER_OPTS exec -t "$CONTAINER_NAME" /bin/bash
