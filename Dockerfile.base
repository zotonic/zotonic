FROM erlang:20.2-alpine

ENV BUILD_APKS="bsd-compat-headers ca-certificates wget curl make gcc musl-dev g++ git"
ENV SHELL="/bin/sh"

WORKDIR /opt/zotonic

# Install Zotonic runtime dependencies.
# Git is necessary because rebar3 compile, which is called by z:compile(),
# requires Git.
RUN apk add --no-cache bash file gettext git imagemagick openssl

COPY docker/zotonic.config /etc/zotonic/zotonic.config
COPY docker/erlang.config.sed /opt/zotonic/docker/erlang.config.sed
COPY apps/zotonic_launcher/priv/config/erlang.config.in /opt/zotonic/apps/zotonic_launcher/priv/config/erlang.config.in

RUN sed -f docker/erlang.config.sed apps/zotonic_launcher/priv/config/erlang.config.in > /etc/zotonic/erlang.config \
    && adduser -S -h /tmp -H -D zotonic \
    && chown -R zotonic /opt/zotonic/apps/zotonic_launcher/priv

VOLUME /etc/zotonic

EXPOSE 8000 8443
