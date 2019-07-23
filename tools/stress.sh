#!/usr/bin/env bash
set -e

# For CPU usage across all cores: run top, then press "shift-i" to toggle irix mode

if [ "$1" == "remote" ]; then
    SERVER="http://lojban.johnjq.com"

    ab -n 10000 -c 100 "$SERVER/grammar/introduction/1/exercises/367/get"
else
    SERVER="http://localhost:8000"

    ab -n 100000 -c 500 "$SERVER/grammar/introduction/1/exercises/367/get"
fi

