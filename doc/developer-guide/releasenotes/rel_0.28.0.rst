.. _rel-0.28.0:

Release 0.28.0
==============

Welcome to Zotonic 0.28.0, released on 1 May, 2017.

Main changes are:

* Added a temporary workaround for ErlyDTL compiler creating too many atoms
  (:issue:`1676`).
* Added reqdata to #dispatch_host notification (:issue:`1664`).
* Fixed non-admin users have no permission to upload file (:issue:`1665`).
* Fixed depcache cleanup causes crash (:issue:`1671`).
* Fixed uploading extremely large files crashes z_file_entry (:issue:`1650`).

Commits since 0.27.0
--------------------

There were 17 commits since release 0.27.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

David de Boer (5):
    * Add 0.27.0 release notes
    * mod_seo: Rename shorturl to shortlink (#1651)
    * core: Upgrade depcache
    * mod_acl_user_groups: Fix non-admins denied permission to upload file (#1665)
    * scripts: Raise maximum number of atoms (#1676)

Dirk Geurs (1):
    * Retain reqdata for use in #dispatch_host notification (#1664)

Marc Worrell (11):
    * Do not crash if resulting image is too big (#1652)
    * Lock new z_stdlib
    * mod_oauth: cleanup templates, add option for 'anonymous' users with only the consumer key/secret. (#1635)
    * mod_video: change log level of 'Video info' lager message.
    * Upgrade s3filez.
    * mod_base_site: add 'showmedia' filter
    * mod_base_site: fix typo.
    * core: remove some throws from z_db, return error tuple instead.
    * mod_survey: add es translation for 'submit'
