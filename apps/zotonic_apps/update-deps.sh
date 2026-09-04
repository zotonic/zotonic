#!/bin/bash

set -euo pipefail

# Ensure that all Zotonic core apps are in the deps of the rebar.config file

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
APPS_DIR="$(cd -- "$SCRIPT_DIR/.." && pwd)"
TEMPLATE="$SCRIPT_DIR/rebar.config.template"
CONFIG="$SCRIPT_DIR/rebar.config"
TEMP_DEPS="$(mktemp "${TMPDIR:-/tmp}/zotonic-apps-deps.XXXXXX")"
TEMP_CONFIG="$(mktemp "${TMPDIR:-/tmp}/zotonic-apps-config.XXXXXX")"

cleanup() {
    rm -f -- "$TEMP_DEPS" "$TEMP_CONFIG"
}
trap cleanup EXIT

export LC_ALL=C
APP_NAMES=()

for app_dir in "$APPS_DIR"/zotonic_*
do
    app="$(basename -- "$app_dir")"

    if [ ! -d "$app_dir" ] || [ ! -f "$app_dir/src/$app.app.src" ]; then
        continue
    fi

    case "$app" in
        zotonic_apps|zotonic_mod_acl_mock|zotonic_site_testsandbox)
            continue
            ;;
    esac

    APP_NAMES+=("$app")
done

if [ "${#APP_NAMES[@]}" -eq 0 ]; then
    echo "No Zotonic applications found in $APPS_DIR" >&2
    exit 1
fi

for ((index = 0; index < ${#APP_NAMES[@]}; index++))
do
    if [ "$index" -lt "$(( ${#APP_NAMES[@]} - 1 ))" ]; then
        printf '%s,\n' "${APP_NAMES[$index]}"
    else
        printf '%s\n' "${APP_NAMES[$index]}"
    fi
done > "$TEMP_DEPS"

sed \
    -e '/ZOTONIC_APPS/ {' \
    -e "r $TEMP_DEPS" \
    -e 'd' \
    -e '}' \
    "$TEMPLATE" > "$TEMP_CONFIG"

cat "$TEMP_CONFIG" > "$CONFIG"
