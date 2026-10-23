#!/bin/bash

# Run from the repository root after building zotonic-docker-test.
# Uses a temporary checkout fixture; no application build or database is needed.
set -euo pipefail

runtime=${1:-docker}
fixture=$(mktemp -d)
project="zotonic-permission-test-$$"
trap 'rm -rf "$fixture"' EXIT
chmod 755 "$fixture"
export ZOTONIC_UID=${ZOTONIC_UID:-$(id -u)}
export ZOTONIC_GID=${ZOTONIC_GID:-$(id -g)}

case "$runtime" in
    docker) compose=(docker compose -f docker-compose.yml) ;;
    podman) compose=(podman-compose -f docker-compose.yml -f docker-compose.podman.yml) ;;
    *) echo 'Usage: bash docker/test-bind-mount.sh [docker|podman]' >&2; exit 1 ;;
esac

mkdir -p "$fixture/docker" "$fixture/docker-data" "$fixture/bin" "$fixture/_build/default/lib/zotonic_core/ebin"
cp docker/*.config "$fixture/docker/"
touch "$fixture/rebar.config" "$fixture/_build/default/lib/zotonic_core/ebin/zotonic_core.app"
cat > "$fixture/bin/zotonic" <<'SH'
#!/bin/sh
set -eu
test "$(id -u)" = "$ZOTONIC_UID"
test "$(id -g)" = "$ZOTONIC_GID"
echo 'Written by zotonic' > /opt/zotonic/host-write-test
SH
chmod +x "$fixture/bin/zotonic"

cat > "$fixture/compose.yml" <<EOF
services:
    zotonic:
        image: zotonic-docker-test
        network_mode: none
        environment:
            FORCE_ZOTONIC: "1"
        volumes:
            - "$fixture:/opt/zotonic:Z"
EOF

cleanup() {
    "${compose[@]}" -f "$fixture/compose.yml" -p "$project" down --volumes >/dev/null 2>&1 || true
    rm -rf "$fixture"
}
trap cleanup EXIT

"${compose[@]}" -f "$fixture/compose.yml" -p "$project" \
    run --rm -T --no-deps zotonic verify
test -O "$fixture/host-write-test"
file_gid=$(stat -c '%g' "$fixture/host-write-test" 2>/dev/null || stat -f '%g' "$fixture/host-write-test")
test "$file_gid" = "$(id -g)"
echo "PASS: $runtime Compose writes files owned by the host user and group"
