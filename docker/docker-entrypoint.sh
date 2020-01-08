#!/bin/sh

# If the command given is a zotonic command, pass it to zotonic; otherwise exec it directly.
# Also check the environment for "FORCE_ZOTONIC" to provide a workaround in case the scripts
# are moved somewhere outside of the path below.
if [ -e "/opt/zotonic/apps/zotonic_launcher/src/command/zotonic_cmd_$1.erl" ] || [ -n "$FORCE_ZOTONIC" ]; then
    set -x

    HOME=/tmp
    ZOTONIC_PIDFILE=/run/zotonic.pid
    ZOTONIC_CONFIG_DIR=/etc/zotonic
    SHELL=/bin/sh

    export HOME ZOTONIC_PIDFILE ZOTONIC_CONFIG_DIR SHELL

    # Create the pid file and enable zotonic to write to it
    touch /run/zotonic.pid && chown zotonic /run/zotonic.pid

    # Allow zotonic to write some state
    mkdir -p /opt/zotonic/priv && chown -R zotonic /opt/zotonic/priv

    # SSL certificates are generated here
    mkdir -p /etc/zotonic/security && chown -R zotonic /etc/zotonic/security

    # Insert password from environment variable into zotonic.config
    sed -i -e "s/{password, \"\"}/{password, \"${ZOTONIC_PASSWORD}\"}/" /etc/zotonic/config.d/docker.config

    # User sites source code is placed here
    chown -R zotonic /opt/zotonic/apps_user

    # And compiled here
    chown -R zotonic /opt/zotonic/_build/default

    exec /usr/bin/gosu zotonic /opt/zotonic/bin/zotonic "$@"
else
    exec "$@"
fi
