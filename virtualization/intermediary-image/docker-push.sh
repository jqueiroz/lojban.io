#!/usr/bin/env bash
set -e

# Workaround for "error creating overlay mount to /var/lib/docker/overlay2/72eb4c1232e563b138b3290af27b3b3dcb898fbc22abf62456557c4c5b81249f/merged: device or resource busy":
# sudo systemctl stop docker
# sudo dockerd -s overlay

# Tag image
docker tag lojban-dependencies johnjq/lojban-tool-dependencies:latest

# Push image
docker push johnjq/lojban-tool-dependencies:latest
