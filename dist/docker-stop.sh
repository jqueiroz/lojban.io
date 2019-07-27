#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/.."

########################## Stop containers ##########################
echo -e ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Stopping containers..."
previously_running=false

if [ -n "`docker $DOCKER_OPTS ps -a | grep lojban-server$`" ]; then
    previously_running=true
    echo -ne "\t"
    docker $DOCKER_OPTS stop lojban-server
fi

echo ""
if [ "$previously_running" = false ]; then
    exit
fi

########################## Remove containers ##########################
echo -e ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Removing containers..."

if [ -n "`docker $DOCKER_OPTS ps -a | grep lojban-server$`" ]; then
    echo -ne "\t"
    docker $DOCKER_OPTS rm lojban-server
fi

echo ""
