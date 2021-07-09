#!/usr/bin/env bash

function install {
    version=$1
    base=https://raw.githubusercontent.com/cotonic/cotonic

    if [ -d priv/lib-src/cotonic/$1 ]; then
        echo " - cotonic ${version} already installed"
    else
        echo " - Downloading cotonic ${version} from github"

        mkdir -p priv/lib-src/cotonic/${version};
        cd priv/lib-src/cotonic/${version};

        curl -#O ${base}/${version}/cotonic.js
        curl -#O ${base}/${version}/cotonic-worker.js
        curl -#O ${base}/${version}/cotonic-service-worker.js

        cd ../../../..
    fi

    mkdir -p priv/lib/cotonic;

    cp priv/lib-src/cotonic/${version}/cotonic.js priv/lib/cotonic/cotonic.js
    cp priv/lib-src/cotonic/${version}/cotonic-worker.js priv/lib/cotonic/cotonic-worker.js
    cp priv/lib-src/cotonic/${version}/cotonic-service-worker.js priv/lib/cotonic/cotonic-service-worker.js
}

install "master" 
