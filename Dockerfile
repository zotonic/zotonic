FROM zotonic/erlang

ADD . /opt/zotonic
WORKDIR /opt/zotonic

COPY docker/zotonic.config /etc/zotonic/zotonic.config

RUN sed -f docker/erlang.config.sed priv/erlang.config.in > /etc/zotonic/erlang.config \
    && adduser -S -h /tmp -H -D zotonic \
    && chown -R zotonic /opt/zotonic/priv

# Note: dumb-init and gosu are pulled from edge; remove that when upgrading to an alpine release that
# includes those packages.
ENV BUILD_APKS="ca-certificates wget curl make gcc musl-dev g++ git"
RUN apk add --no-cache --virtual build-deps $BUILD_APKS \
    && apk add --no-cache bash bsd-compat-headers file gettext imagemagick openssl \
    && apk add --no-cache --repository http://dl-3.alpinelinux.org/alpine/edge/community/ dumb-init \
    && apk add --no-cache --repository http://dl-3.alpinelinux.org/alpine/edge/testing/ gosu \
    && DEBUG=1 make \
    && apk del build-deps

# Use dumb-init to reap zombies, catch signals, and all the other stuff pid 1 should do.
ENTRYPOINT ["/usr/bin/dumb-init", "-c", "--", "/opt/zotonic/docker/docker-entrypoint.sh"]

CMD ["start-nodaemon"]

EXPOSE 8000 8443
