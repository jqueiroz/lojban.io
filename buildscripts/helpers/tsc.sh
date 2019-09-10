#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/../.."

# Run tsc
cd javascript
echo > ../static/scripts/.gitignore
for ts_filename in $(find . -name "*.ts" -not \( -path "*node_modules*" \)); do
    ts_filename="${ts_filename:2}"
    js_filename="${ts_filename/".ts"/".js"}"
    echo "$js_filename" >> ../static/scripts/.gitignore
done
tsc --project "tsconfig.json"

# Copy libraries
cp node_modules/jquery/dist/jquery.min.js ../static/scripts
