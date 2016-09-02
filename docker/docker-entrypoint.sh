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

        # Generate the config file
        sed -e s/%%GENERATED%%/${ZOTONIC_PASSWORD-changeme}/ \
            -e s,%%USER_SITES_DIR%%,/opt/zotonic/user/sites, \
            -e s,%%USER_MODULES_DIR%%,/opt/zotonic/user/modules, \
            -e s,%%USER_EBIN_DIR%%,/opt/zotonic/user/ebin, /opt/zotonic/priv/zotonic.config.in > /etc/zotonic/zotonic.config 

        exec /usr/bin/gosu zotonic /opt/zotonic/bin/zotonic "$@"
else
        exec "$@"
fi

