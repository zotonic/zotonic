#!/bin/bash

# Publish all apps as Hex packages.
# Run release/prepare-release.sh before running this script.
# This script should be running after tagging the master branch with the new version.


# Explicitly set the version of all depending umbrella apps

VERSION=`cat VERSION`

for c in apps/*/rebar.config
do
    sed -i.bck \
    -e "s/zotonic_core,$/{zotonic_core, \"$VERSION\"},/" \
    -e "s/zotonic_notifier,$/{zotonic_notifier, \"$VERSION\"},/" \
    -e "s/zotonic_listen_smtp,$/{zotonic_listen_smtp, \"$VERSION\"},/" \
    -e "s/zotonic_listen_http,$/{zotonic_listen_http, \"$VERSION\"},/" \
    -e "s/zotonic_listen_mqtt,$/{zotonic_listen_mqtt, \"$VERSION\"},/" \
    -e "s/zotonic_filehandler,$/{zotonic_filehandler, \"$VERSION\"},/" \
    -e "s/zotonic_fileindexer,$/{zotonic_fileindexer, \"$VERSION\"},/" \
    -e "s/zotonic_filewatcher,$/{zotonic_filewatcher, \"$VERSION\"},/" \
    -e "s/zotonic_launcher,$/{zotonic_launcher, \"$VERSION\"},/" \
    -e "s/\(zotonic_mod_[a-z_]*\),$/{\1, \"$VERSION\"},/" \
    $c

    sed -i.bck \
    -e "s/zotonic_core$/{zotonic_core, \"$VERSION\"}/" \
    -e "s/zotonic_notifier$/{zotonic_notifier, \"$VERSION\"}/" \
    -e "s/zotonic_listen_smtp$/{zotonic_listen_smtp, \"$VERSION\"}/" \
    -e "s/zotonic_listen_http$/{zotonic_listen_http, \"$VERSION\"}/" \
    -e "s/zotonic_listen_mqtt$/{zotonic_listen_mqtt, \"$VERSION\"}/" \
    -e "s/zotonic_filehandler$/{zotonic_filehandler, \"$VERSION\"}/" \
    -e "s/zotonic_fileindexer$/{zotonic_fileindexer, \"$VERSION\"}/" \
    -e "s/zotonic_filewatcher$/{zotonic_filewatcher, \"$VERSION\"}/" \
    -e "s/zotonic_launcher$/{zotonic_launcher, \"$VERSION\"}/" \
    -e "s/\(zotonic_mod_[a-z_]*\)$/{\1, \"$VERSION\"}/" \
    $c

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
    -e "s/\(zotonic_mod_[a-z_]*\), *\".*\"/\1, \"$VERSION\"/" \
    $c
done


# Delete old artifcats
rm -rf apps/*/_build
rm -f apps/*/rebar.lock

rm -rf release/packages/*/_build
rm -f release/packages/*/rebar.lock


pushd apps;

# First publish core dependencies for other apps

APPS1="zotonic_notifier"

for i in $APPS1
do
    pushd $i

    ../../rebar3 compile
    ../../rebar3 edoc

    ../../rebar3 hex publish -r hexpm --yes

    popd
done

# Wait for Hex to update its index
sleep 20
../rebar3 update

APPS2="zotonic_filewatcher zotonic_fileindexer"

for i in $APPS2
do
    pushd $i

    ../../rebar3 compile
    ../../rebar3 edoc

    ../../rebar3 hex publish -r hexpm --yes

    popd
done

# Wait for Hex to update its index
sleep 20
../rebar3 update

APPS3="zotonic_filehandler"

for i in $APPS3
do
    pushd $i

    ../../rebar3 compile
    ../../rebar3 edoc

    ../../rebar3 hex publish -r hexpm --yes

    popd
done

# Wait for Hex to update its index
sleep 20
../rebar3 update

APPS4="zotonic_core"

for i in $APPS4
do
    pushd $i

    ../../rebar3 compile
    ../../rebar3 edoc

    ../../rebar3 hex publish -r hexpm --yes

    popd
done

# Wait for Hex to update its index
sleep 20
../rebar3 update


APPS5="zotonic_listen_http zotonic_listen_smtp zotonic_listen_mqtt zotonic_mod_admin zotonic_mod_wires"

for i in $APPS5
do
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
done


# Wait for Hex to update its index
sleep 20
../rebar3 update


# Publish all remaining apps - skip core dependencies

for i in *
do
    case $i in
        # APPS1 .. APPS4
        zotonic_notifier)
            ;;
        zotonic_filewatcher)
            ;;
        zotonic_fileindexer)
            ;;
        zotonic_filehandler)
            ;;
        zotonic_core)
            ;;

        # APPS5
        zotonic_listen_http)
            ;;
        zotonic_listen_smtp)
            ;;
        zotonic_listen_mqtt)
            ;;
        zotonic_mod_admin)
            ;;
        zotonic_mod_wires)
            ;;

        # Skip - for testing only
        zotonic_mod_acl_mock)
            ;;
        zotonic_site_testsandbox)
            ;;

        # Publish the remaining sites as packages
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

# Wait for Hex to update its index
sleep 20
../rebar3 update


popd


# Publish the app that includes all zotonic_apps (for easy deps)

pushd release/packages/zotonic_apps

./update-deps.sh
../../../rebar3 compile
../../../rebar3 edoc

../../../rebar3 hex publish -r hexpm --yes

popd


# Cleanup
# rm -rf apps/*/_build
rm -f apps/*/rebar.lock
rm -f apps/*/rebar.config.bck

# rm -rf packages/zotonic_apps/*/_build
rm -f packages/zotonic_apps/*/rebar.lock
