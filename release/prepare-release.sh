#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/.." && pwd)"
RELEASE_HEADER="$REPO_ROOT/apps/zotonic_core/include/zotonic_release.hrl"
VERSION_FILE="$REPO_ROOT/VERSION"

if [ "$#" -ne 1 ]; then
    echo "Usage example: $0 1.2.3"
    exit 1
fi

VERSION="$1"

if [[ ! "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z]+([.-][0-9A-Za-z]+)*)?(\+[0-9A-Za-z]+([.-][0-9A-Za-z]+)*)?$ ]]; then
    echo "Invalid release version: $VERSION" >&2
    echo "Expected a semantic version such as 1.2.3 or 1.2.3-rc.1." >&2
    exit 1
fi

cd "$REPO_ROOT"

if ! git diff --quiet -- "$RELEASE_HEADER" "$VERSION_FILE" \
    || ! git diff --cached --quiet -- "$RELEASE_HEADER" "$VERSION_FILE"
then
    echo "The release version files already contain uncommitted changes." >&2
    echo "Commit or restore them before preparing a release." >&2
    exit 1
fi

# Increments version numbers where needed
# Usage: ./prepare-release.sh 1.2.3

if ! grep -Eq '^-define\(ZOTONIC_VERSION, "[^"]+"\)\.$' "$RELEASE_HEADER"; then
    echo "Could not find ZOTONIC_VERSION in $RELEASE_HEADER" >&2
    exit 1
fi

sed -E -i.bck \
    -e "s/^-define\(ZOTONIC_VERSION, \"[^\"]+\"\)\.$/-define(ZOTONIC_VERSION, \"$VERSION\")./" \
    "$RELEASE_HEADER"
rm -f -- "$RELEASE_HEADER.bck"
printf '%s' "$VERSION" > "$VERSION_FILE"

git add -- "$RELEASE_HEADER" "$VERSION_FILE"
git status
