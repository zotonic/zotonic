.. _rel-0.68.0:

Release 0.68.0
==============

Welcome to Zotonic 0.68.0, released on July 21, 2022.

This is a maintenance release.

Main changes are:

 * Change z_search to return estimated number of rows instead of a count
 * New zotonic config 'filewatcher_terminal_notifier' to disable desktop notifications
 * Fix a problem in controller_static_pages with handling Unicode filenames
 * Change signup-confirmation to require a click on a button before confirming, this
   fixes a problem where confirmations could accidently be done.

Search changes
--------------

The search routines are now using the query planner to estimate the number of rows.

To support this, the following changes were made:

 * The ``all`` field in the ``#search_result`` record is now deprecated.
 * In ``#search_result` there is a new flag ``is_total_estimated``. This is set to ``true``
   if the total was estimated by using the query planner.
 * If the total is estimated then the pager does not show the last page.
 * In the admin the (estimated) total items found is shown below the pager.

With this change some searches will be must faster as the database does not need to count to
max 30K rows, like in the pre 0.68.0 implementation.

The 1.x version of Zotonic is also using the query planner to estimate the number of rows, so
handling this correctly will help you in moving your site to Zotonic 1.x.


Commits since 0.67.0
--------------------

Marc Worrell (10):

 * mod_base: allow controller_static_pages to handle UTF-8 filenames. (#3038)
 * core: error message on gettext po file problems.
 * mod_facebook: use abs urls in head
 * mod_admin: point query args doc to 0.x on Github
 * core: add estimated row counts to search results. (#3041)
 * mod_base: make next/prev pager is page count is undefined. (#3049)
 * core: show more descriptive error on Ajax errors.
 * mod_signup: always press a button to confirm.
 * core: add 'all' to search_result.
 * core: add config 'filewatcher_terminal_notifier' to enable or disable file update growl messages. (#3063)

Rob van den Bogaard (2):

 * Merge pull request #3052 from zotonic/page-unloading-ajax-error
 * Merge pull request #3053 from zotonic/signup-confirm-submit
