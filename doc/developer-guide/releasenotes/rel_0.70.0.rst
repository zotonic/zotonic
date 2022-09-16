.. _rel-0.70.0:

Release 0.70.0
==============

Welcome to Zotonic 0.70.0, released on September 16, 2022.

This is a maintenance release.

Main changes are:

 * Option in mod_auth2fa to force setting 2FA on log on.
 * Fix for sorting menus and category trees, this was caused by the earlier jQuery update.

Commits since 0.69.0
--------------------

Marc Worrell (4):

 * core: change lookahead limit of searches for better pager display.
 * mod_auth2fa: add option to force setting 2FA before log on. (#3122)
 * mod_auth2fa: small fixes.
 * mod_menu: update nestedSortable.js (#3132)

Rob van den Bogaard (1):

 * mod_base: Pager ellipsis link (0.x) (#3120)
