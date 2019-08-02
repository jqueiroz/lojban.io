#!/usr/bin/env bash
set -e

# Change directory to the script's location
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Build: letsencrypt
echo ">>>>>>>>>>>>>> letsencrypt"
./docker.sh $DOCKER_OPTS build -t lojban-letsencrypt-server -f letsencrypt/Dockerfile letsencrypt
echo ""
echo ""

# Build: master-http
echo ">>>>>>>>>>>>>> master-http"
./docker.sh $DOCKER_OPTS build -t lojban-master-http-server -f master-http/Dockerfile master-http
echo ""
echo ""

# Build: master-https
echo ">>>>>>>>>>>>>> master-https"
./docker.sh $DOCKER_OPTS build -t lojban-master-https-server -f master-https/Dockerfile master-https
echo ""
echo ""
