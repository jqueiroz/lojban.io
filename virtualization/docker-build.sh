#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/.."

# Process options
show_usage() {
    echo "Usage: docker-build.sh [-h|--help] [--full]"
}

FULL_BUILD="false"

while :; do
    case $1 in
        -h|--help)
            show_usage
            exit
            ;;
        --full)
            FULL_BUILD="true"
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

# Build docker image
if [[ "$FULL_BUILD" == "true" ]]; then
    ./virtualization/docker.sh build -t lojban-dev-server -f Dockerfile_full .
else
    ./virtualization/docker.sh build -t lojban-dev-server -f Dockerfile .
fi
