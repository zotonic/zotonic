#!/bin/bash

set -x

HOME=/opt/zotonic
ZOTONIC_PIDFILE=/run/zotonic.pid
ZOTONIC_CONFIG_DIR=/etc/zotonic
SHELL=/bin/sh

export HOME ZOTONIC_PIDFILE ZOTONIC_CONFIG_DIR SHELL

# Create the pid file and enable zotonic to write to it
touch /run/zotonic.pid && chown zotonic /run/zotonic.pid

# SSL certificates are generated here
mkdir -p /etc/zotonic/security && chown -R zotonic /etc/zotonic/security

# Directory for configs overruling the zotonic.config file
mkdir -p /etc/zotonic/config.d && chown -R zotonic /etc/zotonic/config.d

# Initialize with some
if [ ! -f "/etc/zotonic/config.d/docker.config" ]
then
    cp ./docker/zotonic-docker.config /etc/zotonic/config.d/docker.config
fi

if [ ! -f "/opt/zotonic/_build/default/lib/zotonic_core/ebin/zotonic_core.app" ]
then
    /usr/bin/gosu zotonic make
fi

# If the command given is a zotonic command, pass it to zotonic; otherwise exec it directly.
# Also check the environment for "FORCE_ZOTONIC" to provide a workaround in case the scripts
# are moved somewhere outside of the path below.
if [ -e "/opt/zotonic/apps/zotonic_launcher/src/command/zotonic_cmd_$1.erl" ] || [ -n "$FORCE_ZOTONIC" ]; then
    exec /usr/bin/gosu zotonic /opt/zotonic/bin/zotonic "$@"
else
    # Start shell

    printf '\n\n####\n#### Usage: bin/zotonic [options] [command]\n####\n#### To rebuild zotonic run make\n####\n\n'
    exec /usr/bin/gosu zotonic /bin/bash
fi
