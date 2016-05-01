#!/bin/sh
set -e

./docker-stop.sh $*
if [ -z "$DOCKER_OPTS" ]; then
    docker $DOCKER_OPTS run -p 80:8000/tcp -it --name lojto-server lojto-server
else
    docker $DOCKER_OPTS run -d --name lojto-server lojto-server
fi
