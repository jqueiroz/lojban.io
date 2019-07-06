#!/usr/bin/env bash
set -e

stack build --fast
./compile-less.sh
stack exec lojto
