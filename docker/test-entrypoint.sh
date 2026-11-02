#!/bin/bash

# Run inside a disposable development image; never mount the checkout here.
# docker build -f docker/Dockerfile.dev -t zotonic-docker-test .
# docker run --rm -i --network none --entrypoint /bin/bash zotonic-docker-test -s < docker/test-entrypoint.sh
# Exercise the real entrypoint, user creation and gosu with a small build fixture.
set -euo pipefail

entrypoint=/opt/zotonic-docker/docker-entrypoint.sh
log=/tmp/entrypoint-test.log
trap 'status=$?; if [ "$status" -ne 0 ]; then cat "$log" >&2; fi' EXIT
touch "$log"
cd /opt/zotonic
mkdir -p docker bin
touch rebar.config docker/rebar.config docker/zotonic-docker.config docker/erlang.config

# Keep the build deterministic and offline, and record which user runs it.
cat > /usr/local/bin/make <<'SH'
#!/bin/sh
id -u > /tmp/build-user
exit "$(cat /tmp/build-status)"
SH
chmod +x /usr/local/bin/make
cat > bin/zotonic <<'SH'
#!/bin/sh
touch /tmp/launcher-called
SH
chmod +x bin/zotonic
echo 0 > /tmp/build-status

run_entrypoint() {
    rm -f /tmp/build-user /tmp/launcher-called
    result=0
    bash "$entrypoint" sh </dev/null >"$log" 2>&1 || result=$?
}

assert_status() {
    if [ "$result" -ne "$1" ]; then
        printf 'Expected exit status %s, got %s\n' "$1" "$result" >&2
        exit 1
    fi
}

assert_permission_failure() {
    assert_status 1
    grep -Fq "Zotonic cannot write to $1 as" "$log"
    grep -Fq 'Docker UID/GID mapping' "$log"
    test ! -e /tmp/build-user
    test ! -e /tmp/launcher-called
}

# Reproduce #3819: root-owned checkout maps to UID 1000, which cannot write.
chmod 755 /opt/zotonic
run_entrypoint
assert_permission_failure /opt/zotonic
test "$(id -u zotonic)" = 1000
test "$(id -g zotonic)" = 1000
echo 'PASS: root-owned checkout fails before build/startup'

# Recreate the user to test the normal non-root UID/GID mapping as well.
userdel zotonic
if getent group zotonic >/dev/null; then
    groupdel zotonic
fi
chown -R 12345:12345 /opt/zotonic /home/zotonic
run_entrypoint
assert_status 0
test "$(id -u zotonic)" = 12345
test "$(id -g zotonic)" = 12345
test "$(cat /tmp/build-user)" = 12345
test -e /tmp/launcher-called
test -z "$(find /opt/zotonic -name '.zotonic-write-test.*' -print)"
echo 'PASS: writable checkout builds as the mapped user and cleans probes'

mkdir -p _build/default/lib
chown -R zotonic:zotonic _build
for directory in _build _build/default _build/default/lib; do
    chown root:root "$directory"
    chmod 755 "$directory"
    run_entrypoint
    assert_permission_failure "/opt/zotonic/$directory"
    chown zotonic:zotonic "$directory"
    echo "PASS: unwritable $directory fails before build/startup"
done

echo 42 > /tmp/build-status
run_entrypoint
assert_status 42
test "$(cat /tmp/build-user)" = 12345
test ! -e /tmp/launcher-called
echo 'PASS: failed build preserves exit status and prevents startup'

# A prebuilt checkout should skip make, even if that command would fail.
mkdir -p _build/default/lib/zotonic_core/ebin
touch _build/default/lib/zotonic_core/ebin/zotonic_core.app
chown -R zotonic:zotonic _build
run_entrypoint
assert_status 0
test ! -e /tmp/build-user
test -e /tmp/launcher-called
echo 'PASS: prebuilt checkout skips make'

# Overrides must win over rebar.config ownership, including in reused containers.
# GID 20 already exists in Debian; no duplicate group should be created.
rm _build/default/lib/zotonic_core/ebin/zotonic_core.app
echo 0 > /tmp/build-status
chown -R 12346:20 /opt/zotonic
chown 12345:12345 rebar.config
ZOTONIC_UID=12346 ZOTONIC_GID=20 run_entrypoint
assert_status 0
test "$(id -u zotonic)" = 12346
test "$(id -g zotonic)" = 20
test "$(cat /tmp/build-user)" = 12346
test "$(id -G zotonic)" = 20
echo 'PASS: overrides update an existing user and reuse an existing group'

# Validate each override before touching accounts or starting the build.
for invalid in 0 -1 abc 01 4294967295; do
    ZOTONIC_UID="$invalid" run_entrypoint
    assert_status 1
    grep -Fq 'ZOTONIC_UID must be' "$log"
    test ! -e /tmp/build-user
    ZOTONIC_GID="$invalid" run_entrypoint
    assert_status 1
    grep -Fq 'ZOTONIC_GID must be' "$log"
    test ! -e /tmp/build-user
done
echo 'PASS: invalid overrides fail before building'

ZOTONIC_UID=1 ZOTONIC_GID=20 run_entrypoint
assert_status 1
grep -Fq 'already used by' "$log"
test ! -e /tmp/build-user
echo 'PASS: an override cannot take over another image account'
