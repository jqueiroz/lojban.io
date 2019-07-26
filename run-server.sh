#!/usr/bin/env bash
set -e

make server
stack exec server
