FROM alpine:3.4

ADD . /opt/zotonic
WORKDIR /opt/zotonic

COPY docker/zotonic.config /etc/zotonic/zotonic.config

RUN sed -f docker/erlang.config.sed priv/erlang.config.in > /etc/zotonic/erlang.config \
    && adduser -S -h /tmp -H -D zotonic \
    && chown -R zotonic /opt/zotonic/priv

# Note: dumb-init and gosu are pulled from edge; remove that when upgrading to an alpine release that
# includes those packages.
RUN apk add --virtual build-deps --no-cache ca-certificates wget curl make gcc musl-dev g++ git \
    && apk add --no-cache bash bsd-compat-headers imagemagick \
    && apk add --no-cache --repository http://dl-3.alpinelinux.org/alpine/edge/community/ dumb-init \
    && apk add --no-cache --repository http://dl-3.alpinelinux.org/alpine/edge/testing/ gosu \
       erlang erlang-inets erlang-compiler erlang-crypto erlang-mnesia erlang-ssl erlang-stdlib erlang-public-key erlang-tools erlang-dev erlang-asn1 erlang-syntax-tools erlang-eunit erlang-parsetools erlang-snmp erlang-sasl erlang-xmerl erlang-erl-interface \
    && DEBUG=1 make \
    && apk del build-deps

# Use dumb-init to reap zombies, catch signals, and all the other stuff pid 1 should do.
ENTRYPOINT ["/usr/bin/dumb-init", "-c", "--", "/opt/zotonic/docker/docker-entrypoint.sh"]

CMD ["start-nodaemon"]

EXPOSE 8000 8443
