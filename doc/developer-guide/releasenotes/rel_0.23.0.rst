.. _rel-0.23.0:

Release 0.23.0
==============

Welcome to Zotonic 0.23.0, released on 5 December, 2016.

Main changes are:

* Added delete interval (and completely disabling deleting) to files stored on
  S3 (:issue:`1493`).
* Added search rank weight configuration parameter (:issue:`1538`).
* Added editing of embedded collections in admin (:issue:`1520`).
* Added support for custom Google Analytics parameters (:issue:`1518`).
* Fixed consistency of log messages and log metadata (:issue:`1510`).
* Fixed ACL checks in admin (:issue:`1514`).
* Fixed embedding not allowed for non-admins (:issue:`1545`).
* Fixed initialization error in mod_acl_user_groups (:issue:`1505`).
* Fixed initial tab on connect modal (:issue:`1497`).

Commits since 0.22.0
--------------------

There were since 22 commits since release 0.22.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

Arjan Scherpenisse (1):
    * scripts: Add -noshell to sitetest command

David de Boer (14):
    * mod_filestore: Add delete interval (#1493)
    * mod_acl_user_groups: Fix #1505 erroneous foreign key
    * Add build deps to environment variable
    * Prepare release 0.22.0 (#1503)
    * mod_filestore: Upgrade version to add deleted column (#1506)
    * admin: Switch position of translation and user menu (fix #1354)
    * admin: Fix access checks (#1514)
    * mod_seo: Support custom Analytics params (#1518)
    * core: Improve log messages consistency (#1510)
    * docker: Switch to Alpine PostgreSQL for smaller file size
    * mod_menu: Fix menu and user groups not editable (#1531)
    * mod_search: Quote search weight and document options
    * mod_base: Validate resource name (#1504)
    * mod_filestore: Fix an issue with setting deletion to 'never' (#1549)

Fred Pook (3):
    * admin: Don't restrict initial tab for zmedia connect dialog (#1497)
    * mod_search - Allow configuration of rank weight
    * mod_editor_tinymce: translation for middle size changed to medium size (#1539)

Marc Worrell (4):
    * Close open db connections on a 'too may connections' error. (#1470)
    * mod_admin: edit collection in block for embedded collections (#1520)
    * core: fix a problem with sanitizing classes in html. (#1526)
    * Fix a problem where a non-admin could not add embedded media. Fixes #1545
