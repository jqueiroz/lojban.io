#!/usr/bin/env bash
set -e

if id -nG | grep -qw "docker"; then
    docker $*
else
    sudo docker $*
fi
