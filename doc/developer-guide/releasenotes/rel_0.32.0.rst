.. _rel-0.32.0:

Release 0.32.0
==============

Welcome to Zotonic 0.32.0, released on 4 September, 2017.

Main changes are:

* Added support for edges in CSV data (:issue:`1690`).
* Sort textual admin search results on relevance (:issue:`1799`).
* Fixed a problem where validation crashes on inputs with a ``:`` in their name (:issue:`1785`).
* Fixed unintentional password changes (:issue:`1801`).

Commits since 0.31.0
--------------------

There were 7 commits since release 0.31.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

David de Boer (3):
    * core: Pass is_import to normalize (#1726)
    * mod_admin_identity: Fix unintentional password changes (fix #1801)
    * tests: Raise timeout to fix build failures (#1806)

Dirk Geurs (1):
    * build: Install openssl in 0.x Docker container (#1792)

Marc Worrell (3):
    * mod_import_csv: add support for incoming/outgoing edges in normal csv data. Issue #1690
    * core: fix a problem where validation crashes on inputs with a ':' in their name (#1785)
    * mod_admin: ensure that text search results are sorted on relevance (#1799)
