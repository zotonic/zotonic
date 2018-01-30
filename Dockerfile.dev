ARG ZOTONIC_VERSION=latest
FROM zotonic/zotonic-base:${ZOTONIC_VERSION}

RUN apk add --no-cache $BUILD_APKS inotify-tools

VOLUME /opt/zotonic
VOLUME /etc/zotonic
