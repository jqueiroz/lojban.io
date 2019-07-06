#!/usr/bin/env bash
set -e

export LANG=en_US.UTF-8
export LC_ALL=C.UTF-8

stack haddock --fast --no-haddock-deps --haddock-internal
