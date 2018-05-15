.. _rel-0.38.0:

Release 0.38.0
==============

Welcome to Zotonic 0.38.0, released on 2 April, 2018.

Main changes are:

* Fixed session cookies missing secure attribute when TLS is terminated by a
  proxy (:issue:`1889`).
* Fixed ``hassubject`` and ``hasobject`` query term parsing (:issue:`1894`,
  :issue:`1896`).

Commits since 0.37.0
--------------------

There were 10 commits since release 0.37.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

David de Boer (2):
    * mod_search: Fix hassubject and hasobject query term parsing (#1894)
    * mod_search: Fix #1895 by also splitting non-binaries (#1896)

Marc Worrell (8):
    * mod_admin_category: fix a problem with deleting pages in a category.
    * Fix messages on category delete error.
    * Lock new z_stdlib
    * Lock new z_stdlib
    * Lock new z_stdlib
    * core: sanitize pivot text before stemming/tsvector. This fixes an issue where illegal utf8 could crash the pivot process.
    * mod_import_wordpress: better image handling. With thanks to @fredpook
    * Set session cookie to secure if site protocol is https. Fixes #1889 (#1890)
