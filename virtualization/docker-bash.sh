#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/.."

# Display logs
./virtualization/docker.sh exec -ti lojban-dev-server /usr/bin/env bash
