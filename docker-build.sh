#!/usr/bin/env bash
set -e

echo ">>>>>>>>>>>>>> lojto"
if [ -z "$1" ]; then
    ./compile-less.sh
    stack build && cp .stack-work/install/x86_64-linux-nix/lts-12.26/*/bin/lojto .docker-binary
    docker $DOCKER_OPTS build -t lojto-server .
    echo ""
    echo ""
elif [ "$1" == "from-source" ]; then
    rm -f .docker-binary
    docker $DOCKER_OPTS build -t lojto-server -f Dockerfile2 .
    echo ""
    echo ""
fi
