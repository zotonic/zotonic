#!/bin/bash

VERSION=`cat ../../../VERSION`

ls ../../../apps \
    | grep -v "zotonic_mod_acl_mock" \
    | grep -v "zotonic_site_testsandbox" \
    | sed -e "s/\(zotonic_[^ ]*\)/{\1, \"$VERSION\"},/g" \
    | sed -E '$ s/},/}/' > tempdeps

cp rebar.config.template rebar.config
sed -i.bck -e '/ZOTONIC_APPS/ {' -e 'r tempdeps' -e 'd' -e '}' rebar.config

rm tempdeps rebar.config.bck
