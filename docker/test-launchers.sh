#!/bin/bash

# Verify host ID defaults and override forwarding without starting containers.
set -euo pipefail
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT
cat > "$fixture/docker" <<'SH'
#!/bin/sh
printf 'uid=%s gid=%s\n' "$ZOTONIC_UID" "$ZOTONIC_GID"
printf '%s\n' "$@"
SH
cp "$fixture/docker" "$fixture/podman-compose"
chmod +x "$fixture/docker" "$fixture/podman-compose"

for launcher in start-docker.sh start-podman.sh; do
    output=$(env -u ZOTONIC_UID -u ZOTONIC_GID PATH="$fixture:$PATH" sh "$launcher")
    test "${output%%$'\n'*}" = "uid=$(id -u) gid=$(id -g)"
    if [ "$launcher" = start-podman.sh ]; then
        grep -Fxq -- '--in-pod=' <<< "$output"
        grep -Fxq './docker-compose.podman.yml' <<< "$output"
    fi
    output=$(env ZOTONIC_UID=12345 ZOTONIC_GID=12346 PATH="$fixture:$PATH" sh "$launcher")
    test "${output%%$'\n'*}" = 'uid=12345 gid=12346'
    output=$(env -u ZOTONIC_GID ZOTONIC_UID=12345 PATH="$fixture:$PATH" sh "$launcher")
    test "${output%%$'\n'*}" = "uid=12345 gid=$(id -g)"
    echo "PASS: $launcher forwards local IDs and explicit overrides"
done
