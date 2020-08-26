#!/usr/bin/env bash
set -e

letsencrypt/letsencrypt-auto certonly \
  --keep-until-expiring --agree-tos --webroot --expand --rsa-key-size 4096 \
  --email contact@lojban.io \
  --webroot-path /letsencrypt-challenge \
  -d lojban.io
