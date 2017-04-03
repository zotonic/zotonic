.. _rel-0.27.0:

Release 0.27.0
==============

Welcome to Zotonic 0.27.0, released on 3 April, 2017.

Main changes are:

* Added platform.twitter.com to whitelist (:issue:`1647`).
* Added config location override based on environment variable (:issue:`1627`).
* Fixed resource creation ACL check missing content group selection
  (:issue:`1640`).

Commits since 0.26.0
--------------------

There were 10 commits since release 0.26.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

David de Boer (2):
    * core: Upgrade filezcache
    * Add 0.26.0 release notes

Maas-Maarten Zeeman (1):
    * core: Add properties to acl insert to allow checks on content/collaboration groups

Marc Worrell (5):
    * core: fix a problem with identifying certain m4a files.
    * mod_editor_tinymce: fix zmedia for Firefox
    * mod_import_csv: fetch more data analyzing the column labels.
    * Facebook now returns JSON during OAuth handshake.
    * Upgrade z_stdlib

Marco Wessel (1):
    * Allow admin to override default config location also for rebar and such (#1627)

loetie (1):
    * [core] Add platform.twitter.com to whitelist (#1647)
