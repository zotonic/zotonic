#!/bin/sh

DOCKER_ARG="$@"
export DOCKER_ARG

exec /bin/s6-svscan /service
