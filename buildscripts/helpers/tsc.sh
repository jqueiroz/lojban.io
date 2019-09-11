#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/../.."

# Compile *.ts files
cd assets/typescript
for ts_filename in $(find . -name "*.ts" -not \( -path "*node_modules*" \)); do
    ts_filename="${ts_filename:2}"
    js_filename="${ts_filename/".ts"/".js"}"
    tsc "$ts_filename" --outFile "../../static/scripts/$js_filename"
done
# tsc --project "tsconfig.json"

# Copy *.js files
cd ../javascript
for js_filename in $(find . -name "*.js"); do
    js_filename="${js_filename:2}"
    cp "$js_filename" "../../static/scripts/$js_filename"
done

# Copy libraries
# cp node_modules/jquery/dist/jquery.min.js ../static/scripts
