#!/usr/bin/env bash
set -e

./buildscripts/stack.sh ghci lojto:lib --ghci-options "-XOverloadedStrings"
