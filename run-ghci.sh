#!/usr/bin/env bash
set -e

./buildscripts/stack.sh ghci lojbanios:lib --ghci-options "-XOverloadedStrings"

# Note: useful command for testing the eberban parser:
# stack --no-nix-pure ghci lojbanios:lib --ghci-options "-XOverloadedStrings -e \"Language.Eberban.Parser.Experimental.parse \\\"test\\\"\""
