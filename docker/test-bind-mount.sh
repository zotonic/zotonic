#!/bin/bash

# Run from the repository root after building zotonic-docker-test.
# Uses a temporary checkout fixture; no application build or database is needed.
# Requires Python 3 with PyYAML to validate and isolate the merged configuration.
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
    podman) compose=(podman-compose --in-pod= -f docker-compose.yml -f docker-compose.podman.yml) ;;
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

# Validate the actual runtime's merge before isolating the Zotonic service.
# podman-compose 1.0.6 retains --requires even with run --no-deps, so remove
# dependencies from the rendered test config instead of relying on that flag.
"${compose[@]}" -f "$fixture/compose.yml" -p "$project" config > "$fixture/merged.yml"
"${PYTHON:-python3}" - "$fixture" <<'PY'
import os
import sys
import yaml

fixture = sys.argv[1]
with open(os.path.join(fixture, "merged.yml")) as source:
    config = yaml.safe_load(source)
    service = config["services"]["zotonic"]
mounts = service["volumes"]
assert len(mounts) == 1, f"Expected one checkout mount, got: {mounts}"
mount = mounts[0]
if isinstance(mount, str):
    assert mount == f"{fixture}:/opt/zotonic:Z", mount
else:
    assert mount["source"] == fixture and mount["target"] == "/opt/zotonic", mount
    assert mount["bind"]["selinux"] == "Z", mount
for key in ("depends_on", "build", "ports", "networks"):
    service.pop(key, None)
with open(os.path.join(fixture, "isolated.yml"), "w") as target:
    isolated = {"services": {"zotonic": service}}
    if "x-podman" in config:
        isolated["x-podman"] = config["x-podman"]
    yaml.safe_dump(isolated, target)
PY

# Keep pod creation disabled for both old and new podman-compose versions.
# The isolated file preserves x-podman settings for newer versions.
case "$runtime" in
    docker) compose=(docker compose) ;;
    podman) compose=(podman-compose --in-pod=) ;;
esac

cleanup() {
    "${compose[@]}" -f "$fixture/isolated.yml" -p "$project" down --volumes >/dev/null 2>&1 || true
    rm -rf "$fixture"
}
trap cleanup EXIT

"${compose[@]}" -f "$fixture/isolated.yml" -p "$project" \
    run --rm -T --no-deps zotonic verify
test -O "$fixture/host-write-test"
file_gid=$(stat -c '%g' "$fixture/host-write-test" 2>/dev/null || stat -f '%g' "$fixture/host-write-test")
test "$file_gid" = "$(id -g)"
echo "PASS: $runtime Compose writes files owned by the host user and group"
