#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/.."

# Process options
show_usage() {
    echo "Usage: docker-run.sh [-h|--help] [-p|--port <port>]"
}

EXTERNAL_PORT="8080"

while :; do
    case $1 in
        -h|--help)
            show_usage
            exit
            ;;
        -p|--port)
            if [ -z "$2" ]; then
                echo "Error: port number is missing"
                exit 1
            fi
            if ! [ "$2" -ge 1 ] 2> /dev/null; then
                echo "Error: port number if not valid"
                exit 1
            fi
            EXTERNAL_PORT="$2"
            shift
            ;;
        --)
            shift
            break
            ;;
        -?*)
            show_usage
            exit 1
            ;;
        *)
            break
    esac

    shift
done

# Ensure that the image exists
if ! docker image ls -a | grep -q "lojban-dev-server"; then
    echo "Failed to start the container -- unable to find docker image: lojban-dev-server"
    echo ""
    echo "Before attempting to start the container, please ensure that the appropriate image is built using: ./virtualization/docker-build.sh"
    exit 1
fi

# Start new containers
./virtualization/docker.sh run -p "$EXTERNAL_PORT":8000/tcp -v /etc/protocols:/etc/protocols:ro --env LOJBANIOS_ENVIRONMENT="dev" --env LOJBANIOS_REDIS_HOSTNAME="127.0.0.1" -it --rm --name lojban-dev-server lojban-dev-server
