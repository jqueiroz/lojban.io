#!/usr/bin/env bash
set -e

echo ">>>>>>>>>>>>>> lojto"
if [ -z "$1" ]; then
    stack build && cp .stack-work/dist/x86_64-*/Cabal*/build/lojto/lojto .docker-binary
    docker $DOCKER_OPTS build -t lojto-server .
    echo ""
    echo ""
elif [ "$1" == "from-source" ]; then
    rm -f .docker-binary
    docker $DOCKER_OPTS build -t lojto-server -f Dockerfile2 .
fi
