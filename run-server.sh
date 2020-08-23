#!/usr/bin/env bash
set -e

if [ -z "$LOJBAN_TOOL_ENVIRONMENT" ]; then
    export LOJBAN_TOOL_ENVIRONMENT="dev"
fi

echo "***** Compiling..."
make server

echo ""
echo "***** Running..."
./buildscripts/stack.sh exec server -- $*
