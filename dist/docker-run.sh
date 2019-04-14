#!/usr/bin/env bash
set -e
./docker-stop.sh $*

echo -e ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Starting containers..."
if [ -z "$DOCKER_OPTS" ]; then
    echo -ne "\t"
    docker $DOCKER_OPTS run -p 80:8000/tcp -it --name lojto-server lojto-server
else
    echo -ne "\t"
    docker $DOCKER_OPTS run -d --name lojto-server lojto-server
fi

echo ""
