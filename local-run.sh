#!/bin/sh
set -e

if [ "$1" == "build" ]; then
    stack build
fi

stack exec lojto
