.. _rel-0.35.0:

Release 0.35.0
==============

Welcome to Zotonic 0.35.0, released on 4 December, 2017.

Main changes are:

* Added automatic pivot table re-creation (:issue:`1735`).
* Added ``match_object_ids`` search term.
* Fixed RSS feeds by serving them with Content-Disposition inline
  (:issue:`1847`).
* Fixed bug on non-SMP systems by enabling SMP by default (:issue:`1490`).
* Fixed WebSocket error messages by correctly closing WebSocket connections
  (:issue:`1840`).
* Fixed inconsistent data by re-pivoting both subject and object when edge is
  inserted/deleted (:issue:`1756`).

Commits since 0.34.0
--------------------

There were 14 commits since release 0.34.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

Dirk Geurs (1):
    * Re-create custom pivot table when definition changes (#1735)

Fred Pook (1):
    * [mod_export] Observe content disposition (#1847)

Maas-Maarten Zeeman (2):
    * core: Carefully close websocket processes instead of killing them. Fixes #1840
    * mod_authentication: Use sudo context to update password

Marc Worrell (10):
    * Always enable smp. Fixes #1490
    * core: remove dependency on zotonic_release.hrl from main zotonic.hrl include.
    * mod_export: don't crash on dispatches that are not export_rsc_query.
    * core: on edge change, pivot both object and subject. Issue #1756. Fixes #1722
    * tinymce/core: add tinymce option to link to url. Manual merge of 9c8964f51fd84e00458a27d8838b0e0c2894ee92
    * core: fix a problem in m_edge where edge-ids were given to a routine handling rsc-ids.
    * mod_search: add  option to match on given object-ids instead of only a subject.
    * mod_search: add 'match_object_ids' search term.
    * mod_search: add 'match_object_ids' search term.
    * mod_editor_tinymce: remove quote at bottom of props dialog
