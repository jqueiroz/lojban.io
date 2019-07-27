#!/usr/bin/env bash
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

run_command(){
    if [[ -n "$LOJBAN_TOOL_BYPASS_NIX" ]]; then
        $1
    else
        nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.03.tar.gz --pure "$DIR/less-environment.nix" --run "$1"
    fi
}

run_lessc() {
    INPUT_FILENAME="$1"
    OUTPUT_FILENAME="$2"
    run_command "lessc "$INPUT_FILENAME" "$OUTPUT_FILENAME""
}

# Setup nix-shell environment
echo "*** Step 1 - Preparing nix-shell environment..."
run_command ""

# Compile .less files
echo "*** Step 2 - Compiling *.less files..."

cd static/style
echo > .gitignore
for less_filename in $(find . -name "*.less"); do
    less_filename="${less_filename:2}"
    css_filename="${less_filename/".less"/".css"}"
    echo "Compiling: $less_filename"
    run_lessc "$less_filename" "$css_filename"
    echo "$css_filename" >> .gitignore
done
