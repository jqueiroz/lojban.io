#!/usr/bin/env bash
set -e

stack build
./compile-less.sh
stack exec lojto
