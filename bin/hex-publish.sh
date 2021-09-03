#!/bin/bash

# Publish all apps as Hex packages.
# Run ./prepare-release.sh before running this script.
# This script should be running after tagging the master branch with the new version.

pushd apps;

# First publish core dependencies for other apps

APPS1="zotonic_notifier zotonic_core"

for i in $APPS1
do
    pushd $i

    ../../rebar3 compile
    ../../rebar3 edoc

    # ../../rebar3 publish

    popd
done

APPS2="zotonic_filewatcher zotonic_filehandler zotonic_fileindexer \
      zotonic_listen_http zotonic_listen_smtp zotonic_listen_mqtt \
      zotonic_launcher"

for i in $APPS2
do
    pushd $i

    # Take zotonic_core as the basis for the build, this prevents
    # fetching and recompiling all dependencies of zotonic_core for
    # every module
    rm -rf _build
    cp -r ../zotonic_core/_build .

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

            # Take zotonic_core as the basis for the build, this prevents
            # fetching and recompiling all dependencies of zotonic_core for
            # every module
            rm -rf _build
            cp -r ../zotonic_core/_build .

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

