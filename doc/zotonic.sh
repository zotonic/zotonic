#!/bin/bash
 
##
## usage zotonic.sh {debug|start|stop|restart}
##
## Use "debug" to start an interactive shell (highly recommended when installing the db on the first run).
## Use "start" in production. This boots zotonic in an erlang vm with a heart beat process.
##
## The proposed directory structure is:
## /home/zotonic/zotonic.sh                         -- this script
## /home/zotonic/zotonic/...                        -- the zotonic code
## /home/zotonic/zotonic/priv/sites/default/...     -- your site and uploaded files go here

# Change this to your base directory
BASE=/home/zotonic

# Change this to the complete path to zotonic.sh (this script)
ZOTONIC_SH=$BASE/zotonic.sh

# Change this to the directory where you have unpacked zotonic
# IMPORTANT: this directory must be called zotonic or zotonic-x.y where x.y is the version number.
ZOTONIC=$BASE/zotonic

# Change this to point to the erlang vm
ERL="/usr/local/bin/erl"

# The include path for the erlang vm, add when needed for your application.
PA="$ZOTONIC/ebin $ZOTONIC/deps/*/ebin"

# The name of the Erlang node, change to 'localhost' when you have problems with your hostname.
HOSTNAME=`hostname`
SNAME=zotonic001

# The command used to restart zotonic when crashed, only used after a "zotonic.sh start"
export HEART_COMMAND="$ZOTONIC_SH start"

## The port and IP address zotonic will bind to (defaults to all ip addresses and port 8000)
export ZOTONIC_IP=any
export ZOTONIC_PORT=8000

pushd $PWD >/dev/null


function start() {
    echo  "Starting zotonic $SNAME"
    make >/dev/null
    $ERL -pa $PA -sname $SNAME -boot start_sasl -heart -detached -s zotonic
}

function stop() {
    echo "Stopping zotonic $SNAME"
    $ERL -noshell -pa $PA -sname ${SNAME}_stop -s zotonic stop $SNAME@$HOSTNAME
}

function update() {
    echo "Updating zotonic $SNAME"
    $ERL -noshell -pa $PA -sname ${SNAME}_stop -s zotonic update $SNAME@$HOSTNAME
}


case $1 in

  start)
    start
    ;;
 
  debug)
    $ERL +P 10000000 +K true -pa $PA -sname $SNAME -boot start_sasl -s zotonic
    ;;
 
  stop)
    stop
    ;;
 
  update)
    update
    ;;

  restart)
    echo "Restarting zotonic"
    stop
    start
    ;;

  *)
    echo "Usage: $0 {debug|start|stop|restart|update}"
    exit 1
esac

popd > /dev/null
 
exit 0

