#!/bin/bash

if [ "$1" = "" ]; then
    echo "Usage example: $0 1.2.3"
    exit 1
fi

VERSION="$1"
MINOR=$(echo $VERSION | sed -e 's/.[^.]*$//')

# Increments version numbers where needed
# Usage: ./prepare-release.sh 1.2.3

sed -i.bck -e "s/^version = .*/version = '$MINOR'/" doc/conf.py
sed -i.bck -e "s/^release = .*/release = '$VERSION'/" doc/conf.py
sed -i.bck -e "s/ZOTONIC_VERSION, .*)./ZOTONIC_VERSION, \"$VERSION\")./" apps/zotonic_core/include/zotonic_release.hrl
echo -n "$VERSION" > VERSION

rm -f doc/conf.py.bck
rm -f apps/zotonic_core/include/zotonic_release.hrl.bck

git add doc/conf.py apps/zotonic_core/include/zotonic_release.hrl VERSION
git status
