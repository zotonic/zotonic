#!/bin/bash

# Increments version numbers where needed
# Usage: ./release.sh 0.13.7

sed -i '' "s/release = .*/release = '$1'/" doc/conf.py
sed -i '' "s/{vsn, .*/{vsn, \"$1\"},/" src/zotonic.app.src
sed -i '' "s/ZOTONIC_VERSION, .*)./ZOTONIC_VERSION, \"$1\")./" include/zotonic_release.hrl
