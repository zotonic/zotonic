#!/bin/sh

# This will start two containers.
#
# - postgres: with PosgreSQL, storing its data on the volume 'pgdata'
# - zotonic: with Erlang and other tools
#
# The Zotonic container mounts the Zotonic "home" directory and
# builds Zotonic in the "./_build/" directory.
#
# You can start Zotonic in the Zotonic container using: "./start.sh"

# Explicit overrides win; otherwise build as the user starting the container.
ZOTONIC_UID=${ZOTONIC_UID:-$(id -u)}
ZOTONIC_GID=${ZOTONIC_GID:-$(id -g)}
export ZOTONIC_UID ZOTONIC_GID

# An empty --in-pod value disables pods on podman-compose 1.0.x.
# Newer versions use x-podman.in_pod in the override file.
NO_PROXY=* podman-compose --in-pod= -f ./docker-compose.yml -f ./docker-compose.podman.yml run --service-ports zotonic sh
