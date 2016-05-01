#!/bin/bash
set -e

echo ">>>>>>>>>>>>>> lojto"
stack build && cp .stack-work/dist/x86_64-linux/Cabal*/build/lojto/lojto .docker-binary
docker $DOCKER_OPTS build -t lojto-server .
echo ""
echo ""
