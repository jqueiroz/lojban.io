#!/usr/bin/env bash
set -e

echo ">>>>>>>>>>>>>> lojto"
docker $DOCKER_OPTS build -t lojto-server -f Dockerfile .
echo ""
echo ""
