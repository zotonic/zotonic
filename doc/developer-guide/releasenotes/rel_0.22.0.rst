.. _rel-0.22.0:

Release 0.22.0
==============

Welcome to Zotonic 0.22.0, released on 7 November, 2016.

Main changes are:

* Added support for managed collaboration rules (:issue:`1492`).
* Added deletion interval for files stored on S3 (:issue:`1493`).
* Fixed database connection recovery after PostgreSQL restart (:issue:`465`).
* Fixed ‘53300 too many connections’ PostgreSQL error (:issue:`1469`).
* Fixed ‘Add connection’ button invisible for link permission (:issue:`1476`).
* Fixed S3 delete requests being indefinitely repeated (:issue:`1488`).

Commits since 0.21.0
--------------------

There were 16 commits since release 0.21.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

David de Boer (8):
    * doc: Add 0.21.0 release notes (#1461)
    * docker: Fix log_dir sed selector
    * core: Fix bcrypt dep on 0.x (#1465)
    * mod_acl_user_groups: Add managed collab rules support (#1492)
    * mod_filestore: Fix #1231 by adding ACL explanation
    * mod_filestore: Add delete interval (#1493)
    * mod_acl_user_groups: Fix #1505 erroneous foreign key
    * Add build deps to environment variable

Dirk Geurs (1):
    * mod_admin: Show add connection buttons if linkable (#1477)

Finn Kruyning (1):
    * Remove TD from admin dashboard

Maas-Maarten Zeeman (2):
    * mod_export: Fix, follow export_resource_visible spec (zotonic_notifications.hrl:847)
    * mod_component: Added the possibility to unmount a component

Marc Worrell (4):
    * core: sanitize Microsoft Office comments, classes and styles. Fixes #1464
    * core: remove trim_all option from binary:split/3 as it is incompatible with OTP 17
    * core: remove transport log messages about unknown page sessions.
    * Close open db connections on a 'too may connections' error. (#1470)
