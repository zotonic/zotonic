#!/usr/bin/env bash

function install {
    url=$1
    file=${url##*/}
    name=${file%.*}
    if [ -d priv/lib/${name} ]; then
        echo " - ${name} already installed"
    else
        echo " - Installing ${name} from $1"
        mkdir -p priv/lib;
        cd priv/lib;
        git clone $1
        echo "Done"
    fi
}

install https://github.com/cotonic/cotonic.git
