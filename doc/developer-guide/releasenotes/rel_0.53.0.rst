.. _rel-0.53.0:

Release 0.53.0
==============

Welcome to Zotonic 0.53.0, released on December 10, 2019.

Main changes are:

 * jQuery updated to 3.4.1
 * Fix a problem with handling email errors
 * Remove a problem with Erlang module name clashes (thanks to @rl-king)
 * Always repivot subject resource on edge delete/insert
 * Fix a problem with displaying images with defined crop-center in the admin
 * Fix a problem where the authors of resource revisions were visible on the revision list for all users

Commits since 0.52.0
--------------------

Maas-Maarten Zeeman (4):

 * Wip blackhole controller (#2236)
 * mod_booststrap: Remove sourceMappingURL properly
 * mod_bootstrap: Ran updated script to remove the sourceMappingURL from the css files
 * mod_base: Update jquery to 3.4.1

Marc Worrell (12):

 * Correct 0.52.0 release date
 * Fix a problem with smtp throws not caught. (#2230)
 * Fix smtp retry logic
 * Fix a problem with log email for cat templates. Fix #2232 (#2233)
 * Merge pull request #2254 from zotonic/wip-upgrade-jquery-3.4.1
 * Merge pull request #2252 from zotonic/wip-fix-bootstrap-update-script
 * Repivot subject on edge delete/insert (#2239)
 * Load google chart via https
 * Fix erlang_localtime and qdate_localtime name clashes (#2263)
 * Do not emit the link-... event if an edge changes (#2268)
 * mod_admin: Fix crop center view in rsc_edge_media.tpl  Fix #2269 (#2270)
 * Only show revisions list if user has edit permissions. (#2273)

loetie (1):

 * Add block around language options (#2271)
