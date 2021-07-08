#!/usr/bin/env bash

function install {
    base=https://raw.githubusercontent.com/cotonic/cotonic/

    if [ -d priv/lib-src/cotonic/$1 ]; then
        echo " - cotonic $1 already installed"
    else
        echo " - Downloading cotonic $1 from github"

        mkdir -p priv/lib-src/cotonic/$1;
        cd priv/lib-src/cotonic/$1;

        curl -#O ${base}/$1/cotonic.js
        curl -#O ${base}/$1/cotonic-worker.js
        curl -#O ${base}/$1/cotonic-service-worker.js

        cd ../../../..
    fi

    mkdir -p priv/lib/cotonic;

    cp priv/lib-src/cotonic/$1/cotonic.js priv/lib/cotonic/cotonic.js
    cp priv/lib-src/cotonic/$1/cotonic-worker.js priv/lib/cotonic/cotonic-worker.js
    cp priv/lib-src/cotonic/$1/cotonic-service-worker.js priv/lib/cotonic/cotonic-service-worker.js
}

install "1.0.6" 
