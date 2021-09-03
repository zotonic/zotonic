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

for c in apps/*/rebar.config
do
    sed -i.bck \
    -e "s/zotonic_core, *\".*\"/zotonic_core, \"$VERSION\"/" \
    -e "s/zotonic_notifier, *\".*\"/zotonic_notifier, \"$VERSION\"/" \
    -e "s/zotonic_listen_smtp, *\".*\"/zotonic_listen_smtp, \"$VERSION\"/" \
    -e "s/zotonic_listen_http, *\".*\"/zotonic_listen_http, \"$VERSION\"/" \
    -e "s/zotonic_listen_mqtt, *\".*\"/zotonic_listen_mqtt, \"$VERSION\"/" \
    -e "s/zotonic_filehandler, *\".*\"/zotonic_filehandler, \"$VERSION\"/" \
    -e "s/zotonic_fileindexer, *\".*\"/zotonic_fileindexer, \"$VERSION\"/" \
    -e "s/zotonic_filewatcher, *\".*\"/zotonic_filewatcher, \"$VERSION\"/" \
    -e "s/zotonic_launcher, *\".*\"/zotonic_launcher, \"$VERSION\"/" \
    $c
done

rm -f doc/conf.py.bck
rm -f apps/zotonic_core/include/zotonic_release.hrl.bck
rm -f apps/*/rebar.config.bck

git add doc/conf.py apps/zotonic_core/include/zotonic_release.hrl VERSION apps/*/rebar.config
git status
