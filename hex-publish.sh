#!/bin/bash

# Publish all apps as Hex packages.
# Run ./prepare-release.sh before running this script.
# This script should be running after tagging the master branch with the new version.


# First publish core dependencies for other apps

APPS="zotonic_notifier zotonic_core \
      zotonic_filewatcher zotonic_filehandler zotonic_fileindexer \
      zotonic_listen_http zotonic_listen_smtp zotonic_listen_mqtt \
      zotonic_launcher"

cd apps;

for i in $APPS
do
    pushd $i

    # Set specific version numbers for all zotonic dependencies in rebar.config

    ../../rebar3 compile
    ../../rebar3 edoc

    # ../../rebar3 publish

    popd
done

# Publish all remaining apps

for i in *
do
    case $i in
        zotonic_notifier)
            ;;
        zotonic_core)
            ;;
        zotonic_filewatcher)
            ;;
        zotonic_filehandler)
            ;;
        zotonic_fileindexer)
            ;;
        zotonic_listen_http)
            ;;
        zotonic_listen_smtp)
            ;;
        zotonic_listen_mqtt)
            ;;
        zotonic_launcher)
            ;;

        *)
            echo $i
            ;;
    esac
done

# Cleanup
#rm -rf apps/*/_build

