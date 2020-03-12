.. _rel-0.55.0:

Release 0.55.0
==============

Welcome to Zotonic 0.55.0, released on March 12, 2020.

Main changes are:

 * All admin pages are now ssl only (if ssl enabled)
 * Warning on admin/status page if the ssl app is not configured
 * Option to start editing languages but not use them yet for the site itself
 * Color coding of the admin depending on the current DTAP environment
 * Show warning in the admin when editing system resources like categories and predicates
 * Reorganization of the new-resource dialog
 * Backoff for erronous task-queue functions
 * Added function z:load_config() to reload the zotonic.config file, this does
   not restart listeners but does change settings like the smtp proxy and DTAP environment.

Commits since 0.54.0
--------------------

Maas-Maarten Zeeman (2):

 * admin: Make all admin page accessible via ssl only. (#2310)
 * 1337 ssl application configuration (#2304)
 * core: Add an explicit table lock to prevent race conditions (#2333)

Marc Worrell (30):

 * Remove X-DTAP-Environment header. Issue #2302
 * Remove X-DTAP-Environment header. Issue #2302 (#2303)
 * docs: Remove mention of x-dtap-environment header.
 * Merge branch '2032-remove-dtap-header' into 0.x
 * Fix bs3 class of progress-bar.
 * mod_translation: add option to make languages available in de admin/edit but not yet enabled. (#2307)
 * mod_admin: Show warning on edit page if editing system content. (#2311)
 * mod_admin: change content warning link to datamodel
 * core: add z:load_config() to reload the zotonic.config file. (#2313)
 * mod_admin: reflect the environment in the admin colors. Issue #2319 (#2320)
 * Hide the 'file upload' on initial show with preselected non-media category. Issue #2305 (#2322)
 * mod_admin: use abs uri for copy of download link
 * core: Add backoff for task queue errors.
 * Add 'name.<name>' as selector for catincludes (#2316)
 * email_status: Mark 'access denied' as unrecoverable.
 * mod_admin: correct menu item text color.
 * mod_admin: small correction in environment.less
 * mod_email_status: add email status panel to email log.
 * Add class 'data-table' which wraps td on any character
 * Fix icons on email status view.
 * mod_email_status: new nl translations
 * mod_logging: new translations
 * email: do not send any email to blocked addresses.
 * mod_admin: new page dialog, swap columns and small changes (#2330)
