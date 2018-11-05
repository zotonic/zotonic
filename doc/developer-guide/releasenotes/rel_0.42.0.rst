.. _rel-0.42.0:

Release 0.42.0
==============

Welcome to Zotonic 0.42.0, released on 5 November, 2018.

Main changes are:

* Prevent search engine indexing of error, logon, and language switch pages.
* Add Yandex site verification
* Support ``user_id`` as configured dispatch id in ``controller_redirect``


Commits since 0.41.0
--------------------

David de Boer (1):

     * mod_admin: Pass content group id when inserting a resource (#1937)

Marc Worrell (9):

     * mod_seo: add Yandex site verification
     * mod_base: support 'user_id' as id value in controller_redirect.
     * mod_seo: allow ovverule of shortlink and cononical url
     * mod_translation: do not index the translation urls, as this will end up in recursions for the crawlers.
     * core: fix a problem where the pivot process could crash if a resource was deleted during pivot.
     * mod_authentication: set 'noindex' and 'notrack' template vars on the logon and error pages.
     * Add 0.42.0 release notes
     * Set version to 0.42.0
     * Merge branch '0.x' into 0.42
