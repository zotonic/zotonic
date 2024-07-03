.. _rel-0.75.0:

Release 0.75.0
==============

Welcome to Zotonic 0.75.0, released on March 2, 2023.

This is a maintenance release.

Main changes are:

 * Support to use Nix for system dependencies
 * Fixes for the pages scomp where wrong URLs could be generated

Commits since 0.74.0
--------------------

Colin de Roos (3):

 * Manage system dependencies with Nix (0.x version) (#3254)
 * Add ps to shell.nix (#3281)
 * mod_cookie_consent: Make the selector for the cookie-consented slightly more general (#3308)

Marc Worrell (5):

 * mod_cookie_consent: fix name of stats field in manage_schema
 * mod_admin: misc fixes for the connect/disconnect dialog (0.x) (#3269)
 * z_search: default pagelen for undefined limit (#3275)
 * Upgrade z_stdlib to a40e135f5288ecdd21b850d2d21b2a8a7769e81d (#3295)
 * mod_base: fix an issue in page scomp where a wrong URL could be generated (0.x) (#3296)
