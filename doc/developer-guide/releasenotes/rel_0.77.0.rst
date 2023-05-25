.. _rel-0.77.0:

Release 0.77.0
==============

Welcome to Zotonic 0.77.0, released on May 25, 2023.

This is a maintenance release.

Main changes are:

 * Fix for handling media classes with non-url-safe characters in the name
 * New filters: url, url_abs.  Both copied over from the master branch.

Commits since 0.76.0
--------------------

Marc Worrell (4):

 * mod_oembed: add oembed support for spotify. (#3403)
 * core: validate mediaclass names (#3405)
 * core: explicit memo-flush of acl permissions after update/insert of rsc
 * mod_base: add filter url and url_abs from master to 0.x (#3425)
