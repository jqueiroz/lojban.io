#!/usr/bin/env python3
import os
import json

# Read Cloudflare token for gamedev zones
with open(os.path.expanduser("~/.cloudflare_lojban_dns_token")) as f:
    token = f.read().strip('\r\n')

CREDS = {
  "cloudflare": {
    "TYPE": "CLOUDFLAREAPI",
    "accountid": "f55dfe88a9bf389c0aed021cc77e7793",
    "apitoken": token,
  },
  "none": { "TYPE": "NONE" },
}

print(json.dumps(CREDS))
