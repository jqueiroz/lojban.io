#!/usr/bin/env bash
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

run_lessc() {
    INPUT_FILENAME="$1"
    OUTPUT_FILENAME="$2"
    lessc "$INPUT_FILENAME" "$OUTPUT_FILENAME"
}

cd static/style
echo > .gitignore
for less_filename in $(find . -name "*.less"); do
    less_filename="${less_filename:2}"
    css_filename="${less_filename/".less"/".css"}"
    echo "Compiling: $less_filename"
    run_lessc "$less_filename" "$css_filename"
    echo "$css_filename" >> .gitignore
done
