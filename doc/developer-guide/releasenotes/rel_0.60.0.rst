.. _rel-0.60.0:

Release 0.60.0
==============

Welcome to Zotonic 0.60.0, released on February 15, 2021.

This is a maintenance release and only includes fixes.

Main changes are:

 * Added support for embedding Soundcloud using OEmbed
 * Fixed a problem where no extension could be found for "application/pgp-keys" .asc files
 * Made the maximum edge list length in the admin configurable using ``mod_admin.edge_list_max_length``
 * Use Github Actions for CI

Commits since 0.59.0
--------------------

Marc Worrell (6):

 * Test build using GitHub Actions. (#2561)
 * Add build batch, remove Travis build batch
 * Add mapping of application/pgp-keys to .asc Fixes #2581
 * Make max edge list length configurable. Fixes #2580
 * Fix typo.
 * Fix Docs GH action.

Rob van den Bogaard (1):

 * Add SoundCloud oembed provider (#2583)

