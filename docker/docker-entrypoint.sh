#!/bin/bash

set -x

HOME=/opt/zotonic
SHELL=/bin/sh

ZOTONIC_PIDFILE=/run/zotonic.pid

ZOTONIC_DIR=/opt/zotonic

ZOTONIC_CONFIG_DIR=$ZOTONIC_DIR/docker-data/config
ZOTONIC_SECURIY_DIR=$ZOTONIC_DIR/docker-data/security
ZOTONIC_DATA_DIR=$ZOTONIC_DIR/docker-data/data
ZOTONIC_LOG_DIR=$ZOTONIC_DIR/docker-data/logs

export HOME ZOTONIC_PIDFILE SHELL
export ZOTONIC_CONFIG_DIR ZOTONIC_SECURITY_DIR ZOTONIC_DATA_DIR ZOTONIC_LOG_DIR

# On macOS the container sees 'root' (uid 0), but the owner on the file
# system is the current user. In that case we can assign the zotonic
# user in the container to any UID.
# On Linux the UID and GID are equal inside and outside the container.

USER_ID=`stat -c '%u' /opt/zotonic/rebar.config`
GROUP_ID=`stat -c '%g' /opt/zotonic/rebar.config`
USER_ID=$([ "$USER_ID" = "0" ] && echo -n "1000" || echo -n "$USER_ID")
GROUP_ID=$([ "$GROUP_ID" = "0" ] && echo -n "1000" || echo -n "$GROUP_ID")

# If this is the initial run, create the Zotonic user.
# Set the user's uid to the same UID as the host user.
if id "zotonic" &>/dev/null
then
    echo "Found user zotonic"
else
    echo "Create user zotonic"
    addgroup -S -g $GROUP_ID zotonic
    adduser -S -D -u $USER_ID -G zotonic zotonic
fi

# Ensure the data and log directories are present and owned by the zotonic user
mkdir -p $ZOTONIC_DATA_DIR && chown -R zotonic $ZOTONIC_DATA_DIR
mkdir -p $ZOTONIC_LOG_DIR && chown -R zotonic $ZOTONIC_LOG_DIR

# Directory for configs overruling the zotonic.config file
mkdir -p $ZOTONIC_CONFIG_DIR/config.d && chown -R zotonic $ZOTONIC_CONFIG_DIR/config.d

# SSL certificates are generated here
mkdir -p $ZOTONIC_SECURIY_DIR && chown -R zotonic $ZOTONIC_SECURIY_DIR

# Create the pid file and enable zotonic to write to it
touch /run/zotonic.pid && chown zotonic /run/zotonic.pid

# Initialize with some default config files
if [ ! -f "/home/zotonic/.config/rebar3/rebar.config" ]
then
    mkdir -p /home/zotonic/.config/rebar3
    cp ./docker/rebar.config /home/zotonic/.config/rebar3/rebar.config
    chown -R zotonic:zotonic /home/zotonic/.config
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
    /usr/bin/gosu zotonic make
fi

# If the command given is a zotonic command, pass it to zotonic; otherwise exec it directly.
# Also check the environment for "FORCE_ZOTONIC" to provide a workaround in case the scripts
# are moved somewhere outside of the path below.
if [ -e "$ZOTONIC_DIR/apps/zotonic_launcher/src/command/zotonic_cmd_$1.erl" ] || [ -n "$FORCE_ZOTONIC" ]; then
    exec /usr/bin/gosu zotonic /opt/zotonic/bin/zotonic "$@"
else
    # Start shell
    bin/zotonic
    printf '\n\n####\n#### Usage: bin/zotonic [options] [command]\n####\n#### To rebuild zotonic run: make\n####\n#### To start in foreground with Erlang terminal: ./start.sh\n####\n\n'
    exec /usr/bin/gosu zotonic /bin/bash
fi
