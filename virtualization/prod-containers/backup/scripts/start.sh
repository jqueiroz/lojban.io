#!/bin/bash
set -e

# Configure s3cmd
echo "[Startup] Configuring s3cmd..."
S3CFG="$(cat /s3cfg_template)"
S3CFG="${S3CFG//<lojbanios_backups_access_key>/$LOJBANIOS_BACKUPS_ACCESS_KEY}"
S3CFG="${S3CFG//<lojbanios_backups_secret_key>/$LOJBANIOS_BACKUPS_SECRET_KEY}"
echo "$S3CFG" > /s3cfg

# Run backup script
#/bin/bash /backup-redis.sh

# Start cron
echo "[Startup] Starting cron..."
cron -f
