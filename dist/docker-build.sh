#!/usr/bin/env bash
set -e

echo ">>>>>>>>>>>>>> lojban"
docker $DOCKER_OPTS build -t lojban-server -f Dockerfile .
echo ""
echo ""
