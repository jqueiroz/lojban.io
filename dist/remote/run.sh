#!/usr/bin/env bash
set -e

# Change directory to the script's location
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Stop: master-http
echo "Stopping: lojban-master-http-server..."
if [ -n "`./docker.sh ps -a | grep lojban-master-http-server$`" ]; then
    previously_running=true
    echo -ne "\t"
    ./docker.sh stop lojban-master-http-server
fi

if [ -n "`./docker.sh ps -a | grep lojban-master-http-server$`" ]; then
    echo -ne "\t"
    ./docker.sh rm lojban-master-http-server
fi

# Run: master-http
echo "Starting: lojban-master-http-server..."
echo -ne "\t"
./docker.sh run -d -p 80:80 --name lojban-master-http-server \
    -v /lojban-letsencrypt-challenge/.well-known:/letsencrypt-challenge/.well-known:ro \
    lojban-master-http-server

# Generate certificates
echo -e "Generating certificates..."
./docker.sh run -it --rm --name lojban-letsencrypt-server \
    -v /lojban-letsencrypt-certificates:/etc/letsencrypt \
    -v /lojban-letsencrypt-challenge:/letsencrypt-challenge \
    lojban-letsencrypt-server

# Stop: master-https
echo "Stopping: lojban-master-https-server..."
if [ -n "`./docker.sh ps -a | grep lojban-master-https-server$`" ]; then
    previously_running=true
    echo -ne "\t"
    ./docker.sh stop lojban-master-https-server
fi

if [ -n "`./docker.sh ps -a | grep lojban-master-https-server$`" ]; then
    echo -ne "\t"
    ./docker.sh rm lojban-master-https-server
fi

# Run: master-https
echo "Starting: lojban-master-https-server..."
echo -ne "\t"
./docker.sh run -d -p 443:443 --name lojban-master-https-server \
    -v /lojban-letsencrypt-challenge/.well-known:/letsencrypt-challenge/.well-known:ro \
    -v /lojban-letsencrypt-certificates:/letsencrypt-certificates:ro \
    --link lojban-server \
    lojban-master-https-server
