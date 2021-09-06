#!/bin/bash

# Publish all apps as Hex packages.
# Run ./prepare-release.sh before running this script.
# This script should be running after tagging the master branch with the new version.


# Explicitly set the version of all depending umbrella apps

VERSION=`cat VERSION`

for c in apps/*/rebar.config
do
    sed -i.bck \
    -e "s/zotonic_core, *\".*\"/zotonic_core, \"$VERSION\"/" \
    -e "s/zotonic_notifier, *\".*\"/zotonic_notifier, \"$VERSION\"/" \
    -e "s/zotonic_listen_smtp, *\".*\"/zotonic_listen_smtp, \"$VERSION\"/" \
    -e "s/zotonic_listen_http, *\".*\"/zotonic_listen_http, \"$VERSION\"/" \
    -e "s/zotonic_listen_mqtt, *\".*\"/zotonic_listen_mqtt, \"$VERSION\"/" \
    -e "s/zotonic_filehandler, *\".*\"/zotonic_filehandler, \"$VERSION\"/" \
    -e "s/zotonic_fileindexer, *\".*\"/zotonic_fileindexer, \"$VERSION\"/" \
    -e "s/zotonic_filewatcher, *\".*\"/zotonic_filewatcher, \"$VERSION\"/" \
    -e "s/zotonic_launcher, *\".*\"/zotonic_launcher, \"$VERSION\"/" \
    $c
done

pushd apps;

# First publish core dependencies for other apps

APPS1="zotonic_notifier zotonic_core"

for i in $APPS1
do
    pushd $i

    ../../rebar3 compile
    ../../rebar3 edoc

    # ../../rebar3 hex publish -r hexpm --yes
    # ../../rebar3 hex publish -r hexpm

    popd
done

APPS2="zotonic_filewatcher zotonic_fileindexer zotonic_filehandler \
      zotonic_listen_http zotonic_listen_smtp zotonic_listen_mqtt \
      zotonic_launcher"

APPS2="zotonic_fileindexer zotonic_filehandler \
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

    # ../../rebar3 hex publish -r hexpm --yes
    ../../rebar3 hex publish -r hexpm

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

            ../../rebar3 hex publish -r hexpm --yes

            popd
            ;;
    esac
done

popd


# Publish the app that includes all zotonic_apps (for easy deps)

pushd packages/zotonic_apps

./update-deps.sh
../../rebar3 compile
../../rebar3 edoc

../../rebar3 hex publish -r hexpm --yes

popd


# Cleanup
rm -rf apps/*/_build
rm -f apps/*/rebar.lock

rm -rf packages/zotonic_apps/*/_build
