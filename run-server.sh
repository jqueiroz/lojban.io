#!/usr/bin/env bash
set -e

if [ -z "$LOJBANIOS_ENVIRONMENT" ]; then
    export LOJBANIOS_ENVIRONMENT="dev"
fi

echo "***** Compiling..."
make server

echo ""
echo "***** Running..."
./buildscripts/stack.sh exec server -- $*
