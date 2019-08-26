.. _rel-0.51.0:

Release 0.51.0
==============

Welcome to Zotonic 0.51.0, released on August 26, 2019.

Main changes are:

  * Added DTAP (Development, Testing etc.) status of site to http headers and ``m.site.environment`
  * Updated PostgreSQL driver ``epgsql``
  * Added *Copy download link* button in admin media panel
  * Fixes to admin connect/create dialog


Commits since 0.50.0
--------------------

Dorien (1):

 * Download link (#2163)

Maas-Maarten Zeeman (1):

 * Renamed all pgsql references to epgsql (#2100)

Marc Worrell (14):

 * mod_logging: set {ssl, any} on client log url
 * mod_base: fix json for controller_error http-status return.
 * Use ubuntu trusty for OTP 18 on Travis (#2116)
 * Use epgsql error record (#2122)
 * mod_admin: in the new-rsc dialog, always show all categories. Fixes #2119
 * Fix double translations in po files.
 * More robust media insert of #upload with data
 * Fix icon paths in mod_artwork font css files. (#2159)
 * Show connected media in tinymce select media dialog. (#2152)
 * Allow pager to accept a list for the 'result' arg. (#2125)
 * scomp pager: fix args for list handling
 * pager: fix html for 1 page result.
 * mod_admin: simplify 'copy download link to clipboard' functionality.
 * Add support for DTAP environment. (#2151)
