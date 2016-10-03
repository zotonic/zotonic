.. _rel-0.21.0:

Release 0.21.0
==============

Welcome to Zotonic 0.21.0, released on 3 October, 2016.

Main changes are:

* Added an ``acl_is_owner`` notification (:issue:`1404`).
* Added a Docker image for Zotonic development (:issue:`1425`).
* Improved Docker images size by switching to Alpine (:issue:`1374`).
* Improved password hashing by switching to bcrypt (:issue:`1390`).
* Improved ``is_visible`` filter to support fulltext search results.
* Moved documentation to Read the Docs (:issue:`1454`).
* Fixed removed observers not being detached (:issue:`1441`).

Commits since 0.20.0
--------------------

There were 31 commits since release 0.20.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

Arjan Scherpenisse (1):
    * core: Add default config for 'setup' application

David de Boer (12):
    * core: Remove rid from maybe_allowed (#1400)
    * tests: Fix m_identity_tests on 0.x (#1415)
    * Add 0.20.0 release notes (#1423)
    * mod_admin: Fix spelling error
    * doc: Add deeplink to custompivot
    * doc: Fix ref
    * docker: Negate paths in .dockerignore to make image even smaller (#1426)
    * docker: Fix compilation by switching bcrypt dep (#1434)
    * doc: Improve site creation (fix #1440)
    * docker: Add dev Docker image (#1425)
    * docker: Fix duplicate config files (#1442)
    * doc: Build 0.x docs on Read the Docs (#1454)

Maas-Maarten Zeeman (1):
    * core: Use bcrypt and erlpass for password hashes (#1390)

Marc Worrell (16):
    * mod_acl_user_groups: make the is_owner check a notification (#1404)
    * mod_acl_user_groups: fix is_owner check.
    * mod_admin: pass args to connect dialog items. Add 'thumbnail-linkable' class.
    * mod_admin: in connect-dialog search result, add the class 'unpublished' for unpublished items.
    * mod_admin: filter connect-list on visibility
    * mod_base: let is_visible filter also accept a list of tuples {Id,Score} as returned by the fulltext search
    * core: add smtp headers from the #email message.
    * mod_video: fix replacement of temp re-render placeholder.
    * i18n: manual merge of #1437
    * mod_oembed: don't embed data for 'link' type (our metadata sniffer is better at this)
    * mod_email_status: export clear_status/2
    * core: fix eacces/eaccess mixup
    * core: fix a problem where removing an exported observe function did not detach the observer. Fixes #1441
    * core: less restrictive receive for module shutdown.
    * core: skip identify errors when extracting mime format.
    * mod_export: catch errors in xlsx export of illegal dates

Marco Wessel (1):
    * docker: Use Alpine Linux for the Docker images (#1374)
