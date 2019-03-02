#!/bin/sh
set -e

if [ "$1" == "build" ]; then
    stack build
fi

./compile-less.sh
stack exec lojto
