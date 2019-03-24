#!/usr/bin/env bash
set -e

cd static/style
echo > .gitignore
for less_filename in $(find . -name "*.less"); do
    less_filename="${less_filename:2}"
    css_filename="${less_filename/".less"/".css"}"
    echo "Compiling: $less_filename"
    lessc "$less_filename" "$css_filename"
    echo "$css_filename" >> .gitignore
done
