.. _rel-0.54.0:

Release 0.54.0
==============

Welcome to Zotonic 0.54.0, released on February 4, 2020.

Main changes are:

 * Now fixed DHE parameters are used for SSL. If there is an
   existing DH file then that file will still be used.
 * Options and configurations to set the HSTS headers
 * Upgraded Locus (MaxMind Geo Location Library), now a license key is mandatory.
   See http://docs.zotonic.com/en/0.x/ref/modules/mod_geoip.html
 * A scomp ``ignore_user_inactivity`` is introduced to disable activity tracking.
   This is useful for pages that should not timeout with their session.
 * Generating the translation ``.pot`` files now needs *administrator* or *use.mod_development* rights.
 * Fixes a problem with LinkedIn OAuth autentication
 * Fixes a problem where password reminders could be sent to non-user accounts.


Commits since 0.53.0
--------------------

Maas-Maarten Zeeman (4):

 * core: Don't close the connection in a beforeunload, but later in an unload. Make sure all ajax requests are async (#2296)
 * 1329 set hsts headers (#2297)
 * 2290 session active (#2291)
 * mod_ssl: Use rfc7919 dhe parameters (#2299)

Marc Worrell (8):

 * mod_geoip: add support for MaxMind license key (#2283)
 * mod_base: safe check on error stack object. Fix #2276
 * mod_base_site: fix Twitter share page link.
 * Fix problem with LinkedIn login. Fix #2277
 * overlay: cancel js close click event.
 * mod_authentication: Only send password reminders to resources with a password identity.
 * mod_translation: demand admin or use.mod_development right to generate .pot files. Issue #2298
 * Upgrade locus to 1.9.0
