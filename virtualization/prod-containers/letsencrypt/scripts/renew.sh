#!/usr/bin/env bash
set -e

letsencrypt/letsencrypt-auto certonly \
  --keep-until-expiring --agree-tos --webroot --expand --rsa-key-size 4096 \
  --email lojban@johnjq.com \
  --webroot-path /letsencrypt-challenge \
  -d lojban.johnjq.com -d lojban.io
