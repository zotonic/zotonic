.. _rel-0.74.0:

Release 0.74.0
==============

Welcome to Zotonic 0.74.0, released on January 16, 2023.

This is a maintenance release.

Main changes are:

 * New notification to continue on signup-page after signup
 * Show versions of modules and Zotonic on /admin/modules
 * Improved hreflang link tags for SEO

Commits since 0.73.0
--------------------

Marc Worrell (10):

 * Tune memory usage on 0.x (#3211)
 * mod_cookie_consent: small css changes (#3210)
 * mod_acl_user_groups: use psql 'any' instead of nested query (0.x) (#3215)
 * mod_admin_modules: show the version of modules in the admin/modules (#3241)
 * Signup confirm redirect (0.x) (#3243)
 * core: flush caches if rollback happens during module schema install. (#3248)
 * mod_admin_modules: add warning if not all modules are running. (#3249)
 * mod_admin: remove compile warning
 * mod_seo: do not set home page to noindex on missing languages (0.x) (#3257)
 * mod_translation: remove title attribute from hreflang link tags

Rob van den Bogaard (1):

 * mod_admin_identity: Require module use rights to view user info data (#3235)
