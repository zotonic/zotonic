#!/usr/bin/env bash

# Download the complete runtime before replacing the installed assets. A failed
# refresh must fail deployment and leave the previous runtime available.
set -euo pipefail
cd -- "$(dirname -- "$0")"

version=master
base="https://raw.githubusercontent.com/cotonic/cotonic/${version}"
staging=$(mktemp -d)
trap 'rm -rf "$staging"' EXIT
files=(cotonic.js cotonic-worker.js cotonic-service-worker.js)

echo " - Downloading cotonic ${version} from github"
for file in "${files[@]}"; do
    curl --fail --show-error --silent --location --output "$staging/$file" "$base/$file"
done

mkdir -p "priv/lib-src/cotonic/$version" priv/lib/cotonic
for file in "${files[@]}"; do
    cp "$staging/$file" "priv/lib-src/cotonic/$version/$file"
    cp "$staging/$file" "priv/lib/cotonic/$file"
done
