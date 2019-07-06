#!/usr/bin/env bash
set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Haskell
echo "=====> Haskell:"
echo "=> Running hlint..."

ALL_FILES_OKAY="true"
for file in $(hlint -j8 --json src | jq --raw-output ".[].file" | uniq); do
    ALL_FILES_OKAY="false"
    echo -e "${RED}$file${NC}"
done

if [ "$ALL_FILES_OKAY" == "true" ]; then
    echo -e "${GREEN}All files passed.${NC}"
else
    echo -e "Lint issues found in the above file(s). Forgot to check hlint?"
    echo -e "Try running: ${BLUE}hlint src${NC}"
fi

# Javascript
echo ""
echo "=====> Javascript:"
echo "=> Checking formatting..."

ALL_FILES_OKAY="true"
for file in $(prettier --list-different "static/scripts/*.js" | grep -v "\.min\.js$"); do
    ALL_FILES_OKAY="false"
    echo -e "${RED}$file${NC}"
done

if [ "$ALL_FILES_OKAY" == "true" ]; then
    echo -e "${GREEN}All files passed.${NC}"
else
    echo -e "Deviations from coding style found in the above file(s). Forgot to run Prettier?"
    echo -e "Try running: ${BLUE}prettier --write static/scripts/*.js${NC}"
fi

# CSS/Less
echo ""
echo "=====> Less:"
echo "=> Checking formatting..."

ALL_FILES_OKAY="true"
for file in $(prettier --list-different "static/style/*.less"); do
    ALL_FILES_OKAY="false"
    echo -e "${RED}$file${NC}"
done

if [ "$ALL_FILES_OKAY" == "true" ]; then
    echo -e "${GREEN}All files passed.${NC}"
else
    echo -e "Deviations from coding style found in the above file(s). Forgot to run Prettier?"
    echo -e "Try running: ${BLUE}prettier --write static/style/*.less${NC}"
fi
