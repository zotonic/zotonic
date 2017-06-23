#!/bin/bash

if [ "$1" = "" ]; then
    echo "Usage example: $0 0.14.0"
    exit 1
fi

VERSION="$1"
MINOR=$(echo $VERSION | sed -e 's/.[^.]*$//')

# Increments version numbers where needed
# Usage: ./prepare-release.sh 0.13.7

sed -e "s/^version = .*/version = '$MINOR'/" -i doc/conf.py
sed -e "s/^release = .*/release = '$VERSION'/" -i doc/conf.py
sed -e "s/{vsn, .*/{vsn, \"$VERSION\"},/" -i apps/zotonic_core/src/zotonic_core.app.src
sed -e "s/ZOTONIC_VERSION, .*)./ZOTONIC_VERSION, \"$VERSION\")./" -i apps/zotonic_core/include/zotonic_release.hrl

git status
