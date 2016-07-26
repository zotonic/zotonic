#!/bin/bash

if [ "$1" = 'debug' ]; then
    zotonic start-nodaemon
    zotonic logtail
elif [ "$1" = 'test' ]; then
    zotonic runtests
else
    exec "$@"
fi

