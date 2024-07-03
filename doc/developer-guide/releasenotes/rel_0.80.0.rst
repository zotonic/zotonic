.. _rel-0.80.0:

Release 0.80.0
==============

Welcome to Zotonic 0.80.0, released on November 21, 2023.

This is a maintenance release.

A change regarding the handling and media has been merged from the master (1.x) branch.

 * support for medium_language property and the media_for_language filter;
 * a medium item added to a tinyMCE body is not automatically added as a depiction;
 * for all media embedded in body or other properties a 'refers' connection is added.

Commits since 0.79.0
--------------------

Marc Worrell (1):

 * mod_admin: add support for medium_language  (adapted from master) (#3599)
