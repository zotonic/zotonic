#!/bin/bash

set -ex

HOME=/opt/zotonic
SHELL=/bin/sh

ZOTONIC_PIDFILE=/run/zotonic.pid

ZOTONIC_DIR=/opt/zotonic

ZOTONIC_CONFIG_DIR=$ZOTONIC_DIR/docker-data/config
ZOTONIC_SECURITY_DIR=$ZOTONIC_DIR/docker-data/security
ZOTONIC_DATA_DIR=$ZOTONIC_DIR/docker-data/data
ZOTONIC_LOG_DIR=$ZOTONIC_DIR/docker-data/logs

export HOME ZOTONIC_PIDFILE SHELL
export ZOTONIC_CONFIG_DIR ZOTONIC_SECURITY_DIR ZOTONIC_DATA_DIR ZOTONIC_LOG_DIR

# Launchers pass the host IDs explicitly. Direct container runs can still infer
# ownership, with UID/GID 1000 as the fallback for root-owned Desktop mounts.
USER_ID=${ZOTONIC_UID:-$(stat -c '%u' "$ZOTONIC_DIR/rebar.config")}
GROUP_ID=${ZOTONIC_GID:-$(stat -c '%g' "$ZOTONIC_DIR/rebar.config")}
if [ -z "${ZOTONIC_UID:-}" ] && [ "$USER_ID" = 0 ]; then USER_ID=1000; fi
if [ -z "${ZOTONIC_GID:-}" ] && [ "$GROUP_ID" = 0 ]; then GROUP_ID=1000; fi

validate_id() {
    if [[ ! "$2" =~ ^[1-9][0-9]{0,9}$ ]] || [ "$2" -gt 4294967294 ]; then
        printf '%s must be a non-zero decimal UID/GID (1..4294967294).\n' "$1" >&2
        exit 1
    fi
}
validate_id ZOTONIC_UID "$USER_ID"
validate_id ZOTONIC_GID "$GROUP_ID"

# Reuse groups already present in the image (e.g. macOS users often have GID 20).
if ! getent group "$GROUP_ID" >/dev/null; then
    if getent group zotonic >/dev/null; then
        groupmod --gid "$GROUP_ID" zotonic
    else
        addgroup --gid "$GROUP_ID" zotonic
    fi
fi

# Do not silently run as another image account when an override collides.
existing_user=$(getent passwd "$USER_ID" | cut -d: -f1)
if [ -n "$existing_user" ] && [ "$existing_user" != zotonic ]; then
    printf 'ZOTONIC_UID %s is already used by %s in the image. Choose another UID.\n' \
        "$USER_ID" "$existing_user" >&2
    exit 1
fi
if id zotonic &>/dev/null; then
    if [ "$(id -u zotonic)" != "$USER_ID" ] || [ "$(id -g zotonic)" != "$GROUP_ID" ]; then
        usermod --uid "$USER_ID" --gid "$GROUP_ID" zotonic
    fi
    # Remove the old Podman root-group workaround from reused containers.
    usermod --groups '' zotonic
else
    adduser --comment "Zotonic user" --disabled-password \
        --uid "$USER_ID" --gid "$GROUP_ID" --shell /bin/bash zotonic
fi

# Check access as the build user, before creating files in the mounted checkout.
# A real write also detects read-only mounts and filesystem mapping restrictions.
check_build_directory() {
    local directory="$1"
    if ! /usr/sbin/gosu zotonic /bin/sh -c '
        probe=$(mktemp -d "$1/.zotonic-write-test.XXXXXX") && rmdir "$probe"
    ' sh "$directory"; then
        printf 'Zotonic cannot write to %s as %s.\n' \
            "$directory" "$(id zotonic)" >&2
        printf 'Check the host checkout ownership, permissions and Docker UID/GID mapping.\n' >&2
        printf 'The checkout and existing _build directories must be writable by the container user.\n' >&2
        exit 1
    fi
}

check_build_directory "$ZOTONIC_DIR"
for directory in "$ZOTONIC_DIR/_build" "$ZOTONIC_DIR/_build/default" "$ZOTONIC_DIR/_build/default/lib"; do
    if [ -d "$directory" ]; then
        check_build_directory "$directory"
    fi
done

# Ensure the config, data and log directories are present and owned by the zotonic user
mkdir -p $ZOTONIC_CONFIG_DIR && chown -R zotonic $ZOTONIC_CONFIG_DIR
mkdir -p $ZOTONIC_DATA_DIR && chown -R zotonic $ZOTONIC_DATA_DIR
mkdir -p $ZOTONIC_LOG_DIR && chown -R zotonic $ZOTONIC_LOG_DIR

# Directory for configs overruling the zotonic.config file
mkdir -p $ZOTONIC_CONFIG_DIR/config.d && chown -R zotonic $ZOTONIC_CONFIG_DIR/config.d

# SSL certificates are generated here
mkdir -p $ZOTONIC_SECURITY_DIR && chown -R zotonic $ZOTONIC_SECURITY_DIR

# Create the pid file and enable zotonic to write to it
touch /run/zotonic.pid && chown zotonic /run/zotonic.pid

# Initialize with some default config files
if [ ! -f "/home/zotonic/.config/rebar3/rebar.config" ]
then
    mkdir -p /home/zotonic/.config/rebar3
    cp ./docker/rebar.config /home/zotonic/.config/rebar3/rebar.config
    chown -R "zotonic:$GROUP_ID" /home/zotonic/.config
fi

if [ ! -f "$ZOTONIC_CONFIG_DIR/config.d/docker.config" ]
then
    cp ./docker/zotonic-docker.config $ZOTONIC_CONFIG_DIR/config.d/docker.config
    chown zotonic $ZOTONIC_CONFIG_DIR/config.d/docker.config
fi

if [ ! -f "$ZOTONIC_CONFIG_DIR/erlang.config" ]
then
    cp ./docker/erlang.config $ZOTONIC_CONFIG_DIR/erlang.config
    chown zotonic $ZOTONIC_CONFIG_DIR/erlang.config
fi

if [ ! -f "$ZOTONIC_DIR/_build/default/lib/zotonic_core/ebin/zotonic_core.app" ]
then
    /usr/sbin/gosu zotonic make
fi


# If the command given is a zotonic command, pass it to zotonic; otherwise exec it directly.
# Also check the environment for "FORCE_ZOTONIC" to provide a workaround in case the scripts
# are moved somewhere outside of the path below.
if [ -e "$ZOTONIC_DIR/apps/zotonic_launcher/src/command/zotonic_cmd_$1.erl" ] || [ -n "$FORCE_ZOTONIC" ]; then
    exec /usr/sbin/gosu zotonic /opt/zotonic/bin/zotonic "$@"
else
    # Start shell
    bin/zotonic
    printf '\n\n####\n#### Usage: bin/zotonic [options] [command]\n####\n#### To rebuild zotonic run: make\n####\n#### To start in foreground with Erlang terminal: ./start.sh\n####\n\n'
    exec /usr/sbin/gosu zotonic /bin/bash
fi
