#!/usr/bin/env bash
set -e

echo "***** Compiling..."
make server

echo ""
echo "***** Running..."
./buildscripts/stack.sh exec server -- $*
