#!/bin/bash

# Publish all apps as Hex packages.
# Run ./prepare-release.sh before running this script.
# This script should be running after tagging the master branch with the new version.


# First publish core dependencies for other apps

APPS="zotonic_notifier zotonic_core \
      zotonic_filewatcher zotonic_filehandler zotonic_fileindexer \
      zotonic_listen_http zotonic_listen_smtp zotonic_listen_mqtt \
      zotonic_launcher"

pushd apps;

for i in $APPS
do
    pushd $i

    ../../rebar3 compile
    ../../rebar3 edoc

    # ../../rebar3 publish

    popd
done

# Publish all remaining apps - skip core dependencies

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
            pushd $i

            ../../rebar3 compile
            ../../rebar3 edoc

            # ../../rebar3 publish

            popd
            ;;
    esac
done

popd

# Cleanup
rm -rf apps/*/_build
rm -f apps/*/rebar.lock

