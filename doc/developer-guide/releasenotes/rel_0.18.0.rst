.. _rel-0.18.0:

Release 0.18.0
==============

Welcome to Zotonic 0.18.0, released on 4 July, 2016.

Main changes are:

* Removed module mod_acl_adminonly from core (:issue:`1289`). Please use
  :ref:`mod_acl_user_groups` instead. If you need mod_acl_adminonly, the module
  is available in a `separate repository <https://github.com/zotonic/mod_acl_adminonly>`_.
* Added compatibility with OPT 19 (:issue:`1323`).
* Added page blocks to the full text search index (issue:`1131`).
* Added support for Google Universal Analytics (:issue:`1281`).
* Added testsandboxdb site for integration tests against a database (:issue:`1235`).
* Fixed multiple images captions (:issue:`1311`).
* Fixed hierarchy tree not updating after edit (:issue:`1293` and :issue:`1309`).
* Fixed inserted video in TinyMCE not shown (:issue:`1304`).
* Fixed anonymous user editing.

Commits since 0.17.0
--------------------

There were 33 commits since release 0.17.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

Arthur Clemens (4):
      mod_mailinglist: fix unsubscribe url syntax
      mod_admin: add param cat_exclude to overview
      mod_admin_identity: use same column headers as overview
      mod_admin: tweak Dutch translation

David de Boer (3):
      tests: Add testsandboxdb site
      core: Fix inotify statup log message
      tests: Make build fail if Zotonic fails to start

Maas-Maarten Zeeman (2):
      mod_ssl: Redirect http and https to the outside ports
      mod_base: Use utf-8 encoding for application/javascript files

Marc Worrell (22):
      core: fix z_module_manager:activate_await/2.     Also fix a problem where upgrading a module could activate that module.     Issue 1235
      core: only start testsandboxdb during tests.
      mod_base: more generic escapejs filter. Issue #1281
      mod_admin: fix is_session_alive status, thanks to @Dirklectisch
      core: let the ACL decide of anonymous users can edit something. Thanks to @Dirklectisch
      Locked new depcache.
      mod_editor_tinymce: fix a problem where setting the options of one media item changed the options of all media item. Fixes #1311
      mod_video_embed: fix an issue where inserting a video inside tinymce doesn't show the video. Fixes #1304
      mod_admin: prevent iframe overflowing from media preview on edit page.
      mod_admin: fix an issue where the menu editor was not updated after creating a new resource. Fixes #1309 Fixes #1293
      mod_admin: in connect dialog, fix enabling upload tab without depiction predicate.
      core: also pivot texts in blocks. Fixes #1113
      core: only install the skeleton modules on initial site install. Fixes #1279
      core: remove debug from pivot
      docs: remove 'resource' from the contactform cookbook. Fixes #1166
      core: fix return value of z_install:install_skeleton_modules/1
      core: pivot block texts to priority B not A.
      mod_base_site: misc fixes for base site.
      mod_survey: set the language of result emails to the default system language. Fixes #1324
      core: in activate_await, check the module process pid, and not the db.     Fixes #1325
      core: fix whereis check in activate_await. Issue #1325
      core: OTP-19 compatibility changes for 0.x (merging from master). Issue #1323 (#1327)

Péter Gömöri (1):
      Fix xref issues (#1315)

Tah Teche (1):
      mod_seo: Switch to Google Universal Analytics
