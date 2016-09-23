#!/bin/sh


# If the command given is a zotonic command, pass it to zotonic; otherwise exec it directly.
if [ -x "/opt/zotonic/src/scripts/zotonic-$1" ]; then
    set -x

    HOME=/tmp
    ZOTONIC_PIDFILE=/tmp/zotonic.pid
    ZOTONIC_CONFIG_DIR=/etc/zotonic

    export HOME ZOTONIC_PIDFILE ZOTONIC_CONFIG_DIR

    # Create the pid file and enable zotonic to write to it
    touch /run/zotonic.pid && chown zotonic /run/zotonic.pid

    # Insert password from environment variable into zotonic.config
    sed -i -e "s/{password, \"\"}/{password, \"${ZOTONIC_PASSWORD}\"}/" \
        /etc/zotonic/zotonic.config

    exec /usr/bin/gosu zotonic /opt/zotonic/bin/zotonic "$@"
else
    exec "$@"
fi
