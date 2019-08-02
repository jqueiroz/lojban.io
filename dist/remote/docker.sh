#!/usr/bin/env bash
set -e

docker -H :5085 $*
# docker $*
