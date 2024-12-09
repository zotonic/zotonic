#!/bin/bash

# Ensure that all Zotonic core apps are in the deps of the rebar.config file

ls .. \
    | grep -v "zotonic_apps" \
    | grep -v "zotonic_mod_acl_mock" \
    | grep -v "zotonic_site_testsandbox" \
    | sed -e "s/\(zotonic_[^ ]*\)/\1,/g" \
    | sed -E '$ s/,//' > tempdeps

cp rebar.config.template rebar.config
sed -i.bck -e '/ZOTONIC_APPS/ {' -e 'r tempdeps' -e 'd' -e '}' rebar.config

rm tempdeps rebar.config.bck
