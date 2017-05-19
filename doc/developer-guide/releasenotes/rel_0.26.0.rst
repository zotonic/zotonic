.. _rel-0.26.0:

Release 0.26.0
==============

Welcome to Zotonic 0.26.0, released on 6 March, 2017.

Main changes are:

* Added support for special characters in z-media caption (:issue:`1618`).
* Added ``facets`` property to ``#search_result{}`` record (:issue:`1606`).
* Fixed zmedia dialog does not open in Firefox (:issue:`1620`).
* Fixed hidden dropdown menu in admin (:issue:`1603`).
* Fixed a problem with UBF-encoded date values.
* Fixed forbidden response from S3 filestore triggering retries.
* Moved filezcache data and journal directories outside the dependency
  directory (:issue:`1601`).

Commits since 0.25.0
--------------------

There were 19 commits since release 0.25.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

David de Boer (5):
    * core: Make pivot index name unique (#1598)
    * Add 0.25.0 release notes (#1600)
    * core: Add facets property to search_result (#1606)
    * core: Move filezcache data dir outside dep dir (#1621)
    * core: Upgrade filezcache

Maas-Maarten Zeeman (1):
    * core: Explicitly set the timezone of the db connection to UTC

Marc Worrell (13):
    * mod_base: in ubf.js fix a problem with encoding date values
    * mod_admin: fix a problem where in tree-lists the dropdown menu was hidden. Fixes #1603
    * Upgrade z_stdlib.
    * New z_stdlib
    * mod_base: add 'is_danger' flag to the confirm dialog. This will show the 'ok' button with the 'btn-danger' style.
    * Add support for special characters in the z-media caption. (#1618)
    * New z_stdlib
    * core: allow m_edge:get_id/4 calls with an non-existing predicate.
    * mod_filestore: handle S3 forbidden as non retryable error.
    * core: fix utf8 problem for R16
    * mod_editor_tinymce: fix check for crop checkbox.
    * core: the medium size is actually called 'middle' (for 0.x)
    * mod_editor_tinymce: fix zmedia for Firefox
