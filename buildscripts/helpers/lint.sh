#!/usr/bin/env bash
set -e

# Change directory to the project's root
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/../.."

# Define colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Run linter for Haskell
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

# Run linter for Javascript
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

# Run linter for CSS/Less
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
