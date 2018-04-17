#!/usr/bin/env bash

function install {
    url=$1
    file=${url##*/}
    name=${file%.*}
    if [ -d priv/lib-src/${name} ]; then
        echo " - ${name} already installed"
    else
        echo " - Installing ${name} from $1"
        mkdir -p priv/lib-src;
        cd priv/lib-src;
        git clone $1
        echo "Done"
        cd ../..
    fi
}

install https://github.com/cotonic/cotonic.git

cd priv/lib-src/cotonic
ZOTONIC_LIB=1 make
