#!/usr/bin/env bash
set -e

export DOCKER_OPTS="-H :5054"
./docker-build.sh $*
