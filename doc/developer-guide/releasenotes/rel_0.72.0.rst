.. _rel-0.72.0:

Release 0.72.0
==============

Welcome to Zotonic 0.72.0, released on November 21, 2022.

This is a maintenance release.

Main changes are:

 * New module ``mod_cookie_consent``
 * Better estimates for the number of found rows in searches

Commits since 0.71.0
--------------------

Marc Worrell (5):

 * Upgrade z_stdlib
 * core: ensure that lager metadata is set for spawned processes and queries (#3167)
 * core: in search allow undefined for pagelen. Issue #3171 (#3172)
 * core: in sql use 'any' instead of 'select unnest' (#3181)
 * mod_cookie_consent: implements cookie consent handling and settings (#3184)
