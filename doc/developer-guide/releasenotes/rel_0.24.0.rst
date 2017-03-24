.. _rel-0.24.0:

Release 0.24.0
==============

Welcome to Zotonic 0.24.0, released on 2 January, 2017.

Main changes are:

* Fixed deletion date 'never' in mod_filestore (:issue:`1549`).
* Fixed handling of illegal Exif data (:issue:`1557`).
* Fixed adding embedded media (:issue:`1545`).
* Fixed live validation message_after error when id is invalid (:issue:`1543`).


Commits since 0.23.0
--------------------

There were since 16 commits since release 0.23.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

David de Boer (4):
    * core: Support custom search total (#1537)
    * mod_filestore: Fix an issue with setting deletion to 'never' (#1549)
    * Add 0.23.0 release notes
    * core: Fix filezcache gzip compress pattern match

Marc Worrell (11):
    * Fix a problem where a non-admin could not add embedded media. Fixes #1545
    * Lock new exif (and bcrypt). Issue #1556
    * mod_media_exif: fix a problem with illegal exif dates. Issue #1557
    * mod_base_site: fix order of includes.
    * mod_base: add UBF support for floats, proplists and dates.
    * Lock new z_stdlib (for z_ubf)
    * mod_base: fix a problem where a transport before init of z_pageid would reload the page.
    * core: add z_mqtt:payload_user/1 function.
    * core: fix a problem with identify of EPS files. Also catch crashes in Exif library on unknown or corrupt exif data.
    * Lock new exif
    * core: fix a problem with pre-page init pubzub subscriptions.

Álvaro Pagliari (1):
    * mod_base: fix livevalidation message_after error when invalid id (#1543)
