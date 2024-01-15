#!/usr/bin/env bash
set -e

ssh lojban@lojban.io -L 5085:127.0.0.1:2375 -N -i ~/.ssh/id_azure
