#!/bin/bash

set -euo pipefail

# Publish all apps as Hex packages.
# Run release/prepare-release.sh before tagging and running this script.
# The checked-out commit must be tagged with the exact version in VERSION.

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/.." && pwd)"
APPS_DIR="$REPO_ROOT/apps"
REBAR3="$REPO_ROOT/rebar3"
VERSION_FILE="$REPO_ROOT/VERSION"
RELEASE_HEADER="$APPS_DIR/zotonic_core/include/zotonic_release.hrl"
INTERNAL_APP_PATTERN='zotonic_core|zotonic_notifier|zotonic_listen_smtp|zotonic_listen_http|zotonic_listen_mqtt|zotonic_filehandler|zotonic_fileindexer|zotonic_filewatcher|zotonic_launcher|zotonic_site_status|zotonic_mod_[a-z0-9_]+'
HEX_INDEX_RETRIES=12
HEX_INDEX_RETRY_DELAY=10

if [ -z "${HEX_API_KEY:-}" ]; then
    echo "HEX_API_KEY was not found; publishing cannot continue." >&2
    echo "Create one with:" >&2
    echo "  ./rebar3 hex user auth" >&2
    echo "  ./rebar3 hex user key generate --key-name zotonic-release --permission api:write" >&2
    echo "Then export the generated key before publishing:" >&2
    echo '  export HEX_API_KEY="<generated-key>"' >&2
    echo "For GitHub Actions, store it as the repository or organization secret HEX_API_KEY." >&2
    exit 1
fi

if [ ! -r "$VERSION_FILE" ]; then
    echo "Missing VERSION file: $VERSION_FILE" >&2
    exit 1
fi

VERSION="$(< "$VERSION_FILE")"
VERSION_PATTERN="${VERSION//./\\.}"
VERSION_PATTERN="${VERSION_PATTERN//+/\\+}"

if [[ ! "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z]+([.-][0-9A-Za-z]+)*)?(\+[0-9A-Za-z]+([.-][0-9A-Za-z]+)*)?$ ]]; then
    echo "Invalid release version in VERSION: $VERSION" >&2
    exit 1
fi

if ! grep -Fqx -- "-define(ZOTONIC_VERSION, \"$VERSION\")." "$RELEASE_HEADER"; then
    echo "VERSION and ZOTONIC_VERSION do not match." >&2
    echo "Run release/prepare-release.sh $VERSION before tagging the release." >&2
    exit 1
fi

if ! git -C "$REPO_ROOT" tag --points-at HEAD | grep -Fqx -- "$VERSION"; then
    echo "The current commit is not tagged with the release version $VERSION." >&2
    echo "Check out the exact release tag before publishing." >&2
    exit 1
fi

if [ -n "$(git -C "$REPO_ROOT" status --porcelain --untracked-files=normal)" ]; then
    echo "The release checkout contains uncommitted or untracked files." >&2
    echo "Publishing is only allowed from a clean release tag." >&2
    exit 1
fi

cd "$REPO_ROOT"
export LC_ALL=C

RELEASE_TEMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/zotonic-hex-publish.XXXXXX")"
BACKUP_DIR="$RELEASE_TEMP_DIR/original"
BUILD_DIR="$RELEASE_TEMP_DIR/build"
INDEX_BUILD_DIR="$BUILD_DIR/index"
APP_CONFIGS=("$APPS_DIR"/*/rebar.config)
BACKED_UP_CONFIGS=()

cleanup() {
    local status=$?
    local config app_dir app app_backup_dir
    local cleanup_failed=false
    trap - EXIT
    trap - HUP INT TERM
    set +e

    for config in "${BACKED_UP_CONFIGS[@]}"
    do
        app_dir="$(dirname -- "$config")"
        app="$(basename -- "$app_dir")"
        app_backup_dir="$BACKUP_DIR/$app"

        if [ -f "$app_backup_dir/rebar.config" ]; then
            cp -p "$app_backup_dir/rebar.config" "$config" || cleanup_failed=true
        fi

        if [ -f "$app_backup_dir/had-rebar-lock" ]; then
            if [ -f "$app_backup_dir/rebar.lock" ]; then
                cp -p "$app_backup_dir/rebar.lock" "$app_dir/rebar.lock" || cleanup_failed=true
            else
                cleanup_failed=true
            fi
        else
            rm -f -- "$app_dir/rebar.lock" || cleanup_failed=true
        fi

        rm -f -- "$config.bck" || cleanup_failed=true
    done

    if [ "$cleanup_failed" = false ]; then
        rm -rf -- "$RELEASE_TEMP_DIR"
    else
        echo "Could not restore all release files; backups remain in $BACKUP_DIR" >&2
        if [ "$status" -eq 0 ]; then
            status=1
        fi
    fi

    exit "$status"
}
trap cleanup EXIT
trap 'exit 130' HUP INT TERM

for config in "${APP_CONFIGS[@]}"
do
    app_dir="$(dirname -- "$config")"
    app="$(basename -- "$app_dir")"
    app_backup_dir="$BACKUP_DIR/$app"

    mkdir -p "$app_backup_dir"
    cp -p "$config" "$app_backup_dir/rebar.config"

    if [ -f "$app_dir/rebar.lock" ]; then
        touch "$app_backup_dir/had-rebar-lock"
    fi

    BACKED_UP_CONFIGS+=("$config")

    if [ -f "$app_backup_dir/had-rebar-lock" ]; then
        cp -p "$app_dir/rebar.lock" "$app_backup_dir/rebar.lock"
        rm -f -- "$app_dir/rebar.lock"
    fi
done

# Ensure that zotonic_apps contains every publishable Zotonic application.
"$APPS_DIR/zotonic_apps/update-deps.sh"

# Hex packages must reference the exact matching versions of internal apps.
for config in "${APP_CONFIGS[@]}"
do
    sed -E -i.bck \
        -e "s/^([[:space:]]*)(${INTERNAL_APP_PATTERN}),$/\\1{\\2, \"$VERSION\"},/" \
        -e "s/^([[:space:]]*)(${INTERNAL_APP_PATTERN})$/\\1{\\2, \"$VERSION\"}/" \
        -e "s/^([[:space:]]*)\\{(${INTERNAL_APP_PATTERN}),[[:space:]]*\"[^\"]+\"\\}(,?)$/\\1{\\2, \"$VERSION\"}\\3/" \
        "$config"
    rm -f -- "$config.bck"
done

if grep -En "^[[:space:]]*(${INTERNAL_APP_PATTERN}),?[[:space:]]*$" "${APP_CONFIGS[@]}"; then
    echo "Found unversioned internal Zotonic dependencies after rewriting rebar.config files." >&2
    exit 1
fi

publish_app() {
    local app="$1"
    local reuse_core_build="${2:-false}"
    local app_dir="$APPS_DIR/$app"
    local app_build_dir="$BUILD_DIR/$app"

    if [ ! -f "$app_dir/rebar.config" ]; then
        echo "Missing application or rebar.config: $app" >&2
        return 1
    fi

    mkdir -p "$app_build_dir"

    if [ "$reuse_core_build" = true ]; then
        if [ ! -d "$BUILD_DIR/zotonic_core" ]; then
            echo "The zotonic_core build is not available for $app." >&2
            return 1
        fi
        cp -R "$BUILD_DIR/zotonic_core/." "$app_build_dir/"
    fi

    echo "Publishing $app $VERSION"
    (
        cd "$app_dir"
        REBAR_BASE_DIR="$app_build_dir" "$REBAR3" compile
        REBAR_BASE_DIR="$app_build_dir" "$REBAR3" hex publish -r hexpm --yes
    )
}

wait_for_hex_packages() {
    local -a packages=("$@")
    local attempt package package_info
    local all_found

    for ((attempt = 1; attempt <= HEX_INDEX_RETRIES; attempt++))
    do
        all_found=true

        if ! REBAR_BASE_DIR="$INDEX_BUILD_DIR" "$REBAR3" update; then
            all_found=false
        else
            for package in "${packages[@]}"
            do
                if ! package_info="$(REBAR_BASE_DIR="$INDEX_BUILD_DIR" "$REBAR3" pkgs "$package" 2>/dev/null)" \
                    || ! grep -Eq -- "(^|[^0-9A-Za-z.+-])${VERSION_PATTERN}([^0-9A-Za-z.+-]|$)" <<< "$package_info"
                then
                    all_found=false
                    break
                fi
            done
        fi

        if [ "$all_found" = true ]; then
            return 0
        fi

        if [ "$attempt" -lt "$HEX_INDEX_RETRIES" ]; then
            echo "Waiting for Hex to index $VERSION (attempt $attempt/$HEX_INDEX_RETRIES)..."
            sleep "$HEX_INDEX_RETRY_DELAY"
        fi
    done

    echo "Hex did not index all required $VERSION packages in time: ${packages[*]}" >&2
    return 1
}

# Publish foundational applications before packages that depend on them.
APPS1=(zotonic_notifier)
for app in "${APPS1[@]}"
do
    publish_app "$app"
done
wait_for_hex_packages "${APPS1[@]}"

APPS2=(zotonic_filewatcher zotonic_fileindexer)
for app in "${APPS2[@]}"
do
    publish_app "$app"
done
wait_for_hex_packages "${APPS2[@]}"

APPS3=(zotonic_filehandler)
for app in "${APPS3[@]}"
do
    publish_app "$app"
done
wait_for_hex_packages "${APPS3[@]}"

APPS4=(zotonic_core)
for app in "${APPS4[@]}"
do
    publish_app "$app"
done
wait_for_hex_packages "${APPS4[@]}"

APPS5=(
    zotonic_listen_http
    zotonic_listen_smtp
    zotonic_listen_mqtt
    zotonic_mod_admin
    zotonic_mod_wires
)
for app in "${APPS5[@]}"
do
    publish_app "$app" true
done
wait_for_hex_packages "${APPS5[@]}"

# Publish all remaining applications after their shared dependencies.
PUBLISHED_REMAINING=()
for app_dir in "$APPS_DIR"/*
do
    if [ ! -d "$app_dir" ] || [ ! -f "$app_dir/rebar.config" ]; then
        continue
    fi

    app="$(basename -- "$app_dir")"

    case "$app" in
        zotonic_apps|zotonic_notifier|zotonic_filewatcher|zotonic_fileindexer|\
        zotonic_filehandler|zotonic_core|zotonic_listen_http|zotonic_listen_smtp|\
        zotonic_listen_mqtt|zotonic_mod_admin|zotonic_mod_wires|\
        zotonic_mod_acl_mock|zotonic_site_testsandbox)
            continue
            ;;
    esac

    publish_app "$app" true
    PUBLISHED_REMAINING+=("$app")
done
wait_for_hex_packages "${PUBLISHED_REMAINING[@]}"

# Publish the aggregate package only after all of its dependencies are indexed.
publish_app zotonic_apps true

echo "Published all Zotonic $VERSION packages successfully."
