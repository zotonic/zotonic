ARG ZOTONIC_VERSION=latest
FROM zotonic/zotonic-base:${ZOTONIC_VERSION}

ADD . /opt/zotonic
WORKDIR /opt/zotonic

# Note: gosu is pulled from edge; remove that when upgrading to an alpine release that
# includes the package.
RUN apk add --no-cache --virtual build-deps $BUILD_APKS \
    && apk add --no-cache dumb-init \
    && apk add --no-cache --repository http://dl-3.alpinelinux.org/alpine/edge/testing/ gosu \
    && DEBUG=1 make \
    && apk del build-deps

# Use dumb-init to reap zombies, catch signals, and all the other stuff pid 1 should do.
ENTRYPOINT ["/usr/bin/dumb-init", "-c", "--", "/opt/zotonic/docker/docker-entrypoint.sh"]

CMD ["start-nodaemon"]
