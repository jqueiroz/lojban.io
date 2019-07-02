#!/usr/bin/env bash
set -e

ssh root@files.johnjq.com -L 5054:127.0.0.1:2375 -N
