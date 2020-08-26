#!/usr/bin/env bash
set -e

./buildscripts/stack.sh ghci lojbanios:lib --ghci-options "-XOverloadedStrings"
