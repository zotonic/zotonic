ARG ZOTONIC_VERSION=latest
FROM zotonic/zotonic:${ZOTONIC_VERSION}

# This Docker image extends the regular Zotonic image to add
# a PostgresQL database and, since multiple processes are run,
# process supervision. When run, it considers Zotonic the "main"
# process, and exits if it does.

RUN apk --no-cache add inotify-tools postgresql postgresql-client s6 \
    && gosu postgres initdb -D /var/lib/postgresql -U zotonic

RUN mkdir /run/postgresql && chown postgres /run/postgresql

COPY docker/s6-service /service
COPY docker/docker-entrypoint-heavy.sh /opt/zotonic/docker
COPY docker/zotonic-heavy.config /etc/zotonic/zotonic.config

VOLUME /var/lib/postgresql

# Use dumb-init to reap zombies, catch signals, and all the other stuff pid 1 should do.
ENTRYPOINT ["/usr/bin/dumb-init", "-c", "--", "/opt/zotonic/docker/docker-entrypoint-heavy.sh"]

CMD ["start-nodaemon"]
