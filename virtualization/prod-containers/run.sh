#!/usr/bin/env bash
set -e

# Change directory to the script's location
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Stop: redis
echo "Stopping: lojban-redis-server..."
if [ -n "`./docker.sh ps -a | grep lojban-redis-server$`" ]; then
    previously_running=true
    echo -ne "\t"
    ./docker.sh stop lojban-redis-server
fi

if [ -n "`./docker.sh ps -a | grep lojban-redis-server$`" ]; then
    echo -ne "\t"
    ./docker.sh rm lojban-redis-server
fi

# Run: redis
echo "Starting: lojban-redis-server..."
echo -ne "\t"
./docker.sh run -d --name lojban-redis-server \
    -v /lojban-redis-database/:/database \
    lojban-redis-server

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

# Read credentials for the lojbanios-backups bucket in B2
echo "Reading credentials for the lojbanios-backups bucket in B2..."
LOJBANIOS_BACKUPS_ACCESS_KEY="$(sed -n 1p ~/.lojbanios_b2_app_key)"
LOJBANIOS_BACKUPS_SECRET_KEY="$(sed -n 2p ~/.lojbanios_b2_app_key)"

# Stop: backup
echo "Stopping: lojban-backup-server..."
if [ -n "`./docker.sh ps -a | grep lojban-backup-server$`" ]; then
    previously_running=true
    echo -ne "\t"
    ./docker.sh stop lojban-backup-server
fi

if [ -n "`./docker.sh ps -a | grep lojban-backup-server$`" ]; then
    echo -ne "\t"
    ./docker.sh rm lojban-backup-server
fi

# Run: backup
echo "Starting: lojban-backup-server..."
echo -ne "\t"
./docker.sh run -d --name lojban-backup-server \
    --env LOJBANIOS_BACKUPS_ACCESS_KEY="$LOJBANIOS_BACKUPS_ACCESS_KEY" \
    --env LOJBANIOS_BACKUPS_SECRET_KEY="$LOJBANIOS_BACKUPS_SECRET_KEY" \
    -v /lojban-backup/:/backup \
    -v /lojban-redis-database/:/redis-database:ro \
    lojban-backup-server
