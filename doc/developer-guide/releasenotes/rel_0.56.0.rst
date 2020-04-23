.. _rel-0.56.0:

Release 0.56.0
==============

Welcome to Zotonic 0.56.0, released on April 23, 2020.

Possible breaking changes:

 * We removed jquery-migrate from the standard template in
   ``modules/mod_base/templates/_js_include_jquery.tpl``.
   If you need jquery migrate in your project then you can add
   this template to your site and re-add ``"js/apps/jquery-migrate-1.4.1.min.js"``
   to the include library files.
 * The ``{% image %}`` tag now includes ``width`` and ``height`` attributes.
   Ensure that you have width or height set to *auto* in your css if you are
   restricting one of the dimensions. Otherwise use the new ``nowh`` option
   to the image tag to suppress the width/height attributes.

Main changes are:

 * Added tracing of database queries - switch on at the /admin/development
   and see SQL traces (and explain of slow queries) for your session only
   in the console log.
 * Removed jquery-migrate from the standard jquery include
 * Image tags now include width/height attributes
 * Language preference is now stored in ``z.lang`` cookie instead of the
   database persistent storage. This greatly reduces the number of rows
   being added to the persistent storage table.
 * SQL queries that timeout are now canceled, the query is now also
   monitored and logged in case of crashes or timeouts.
 * If an email is received for an unknown hostname then a temporary
   error is returned if and only if there are enabled sites that are not
   running.
 * Transport retransmission is now disabled. This turned out to cause
   more problems than solve, due to duplicate requests being sent in case
   of slow servers, making the server even slower.
 * Pivot queue inserts are now more robust and handled by the pivot
   server process, this prevents race conditions.
 * The site supervisor is reorganized for greater robustness against
   crashing of some services. These services are now organized in their
   own one-for-one supervisor and are independently restarted.
 * Next/prev on the admin edit page is now evaluated when clicked. This
   keeps the correct category and speeds up page load of the edit page.
 * Long connection lists on the admin/edit page are now truncated and
   can be viewed in full on the connections page.
 * File modification times are now cached, this speeds up the template
   modification checks on busy servers.


Commits since 0.55.0
--------------------

Maas-Maarten Zeeman (2):

 * core: Let the pivot queue server insert new edges into the db (#2379)
 * core: Add a database pool usage warning (#2382)

Marc Worrell (35):

 * core: generate image width and height attributes. Issue #2306 (#2318)
 * Remove dependency on $.browser in zotonic js
 * Remove msie test in z.popupwindow.js
 * Cleanup some js, remove jquery.ba-hashchange.js
 * Remove deprecated '.load()' call from z.cropcenter.js
 * Remove jquery migrate from default js jquery include
 * Preliminary 0.56 docs
 * Also document changed w/h attributes on the image tag
 * Add 'loading' attribute to allowed image tag opts.
 * Merge pull request #2352 from zotonic/jq-migrate-0x
 * core: export z_email_receive:parse_email/4
 * Fix problem with sending emails due to string/binary conversions. Upgrade z_stdlib
 * Merge pull request #2356 from zotonic/email-patch-0.55
 * Fix problem with sending emails due to string/binary conversions. Upgrade z_stdlib
 * Release 0.55.1
 * Merge pull request #2357 from zotonic/release-0.55.1
 * Cleanup code.
 * mod_translation: fix a problem setting the default lannguage
 * mod_base: sanitize redirect location for action redirect
 * mod_admin: replace next/prev on admin with lazy search. Issue #2375 (#2376)
 * Use z.lang cookie to store the language preference. Fix #2374 (#2378)
 * SMTP: give 451 error if unknown host and not all sites are running
 * Make pivot queue insert more robust and handle batches of ids.
 * Cache file modification time lookups (#2384)
 * Export pivot function, more conservative truncate of pivot title.
 * With filter lower and upper, sanitize input on failure.
 * Disable zotonic transport retransmission.
 * Truncate long connections lists in the admin. Issue #2387 (0.x) (#2388)
 * Trace database queries. Issue #2390 (#2391)
 * Reorg site supervisor (0.x) (#2386)
 * Allow to change category if category does not exist. Issue #2360 (0.x) (#2389)
 * Catch throw on creation of pages or predicates.
 * Merge pull request #2397 from zotonic/page-create-error-0x
 * z_db_pgsql: fix types.
 * Fix spanish translation.
