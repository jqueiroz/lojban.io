#!/bin/bash
set -e

# Retrieve current time
TIME=`date +"%Y-%m-%d_%H-%M-%S"`

# Prepare local backup
echo "[$TIME] Preparing for backup..."
rm -rf /tmp/backup/redis
mkdir -p /tmp/backup/redis/$TIME/
cp -R /redis-database /tmp/backup/redis/$TIME/
(mkdir -p /backup/redis/ && cd /tmp/backup/redis/ && tar cfzp "/backup/redis/$TIME.tgz" "$TIME")
echo "[$TIME] Successfully generated local backup..."

# Upload backup to s2
echo "[$TIME] Uploading backup to b2..."
s3cmd -c /s3cfg put "/backup/redis/$TIME.tgz" "s3://lojbanios-backups/redis/$TIME.tgz"
