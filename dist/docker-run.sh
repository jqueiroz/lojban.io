#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/.."

# Stop running containers
./dist/docker-stop.sh $*

# Start new containers
echo -e ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Starting containers..."
if [ -z "$DOCKER_OPTS" ]; then
    echo -ne "\t"
    docker $DOCKER_OPTS run -p 80:8000/tcp -it --name lojban-server lojban-server
else
    echo -ne "\t"
    docker $DOCKER_OPTS run -d --name lojban-server lojban-server
fi

echo ""
