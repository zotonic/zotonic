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

NO_PROXY=* docker compose -f ./docker-compose.yml run --service-ports zotonic sh
