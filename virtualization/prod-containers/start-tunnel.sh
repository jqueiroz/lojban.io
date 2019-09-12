#!/usr/bin/env bash
set -e

ssh lojban@lojban.johnjq.com -L 5085:127.0.0.1:2375 -N
