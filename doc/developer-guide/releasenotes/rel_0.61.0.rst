.. _rel-0.61.0:

Release 0.61.0
==============

Welcome to Zotonic 0.61.0, released on June 7, 2021.

This is a maintenance release.

Main changes are:

 * Changes ``mod_seo_sitemap`` module for sites with many pages and/or multiple sources of pages
 * Fix for a problem where database connections were left idling if a timeout happened during a transaction
 * Fix for a problem where the ``zotonic stop`` command could take a long time
 * Support for Google Analytics new "G-" codes
 * Support for Google Tag Manager
 * Make it possible for an authenticated user to change their password on ``/logon/change``

Commits since 0.60.0
--------------------

Dorien (2):

 * Refactor permissions to check use of mod_logging (#2606)
 * Fix #1628: Use summary filter for meta description (#2605)

Marc Worrell (12):

 * SEO GTM support and misc fixes (#2652)
 * Add password change for current user (#2654)
 * Info message if an url was dropped from a tweet.
 * Add placeholders to password change input fields.
 * New sitemap building using urlsets.
 * Delete docs for sitemap.xml.tpl
 * Add SEO sitemap docs.
 * Add comments and license info. Delete empty SEO controllers.
 * Better 'language' handling of seo_sitemap.
 * Remove debug, fix urlset xml
 * Fix a problem with connections lingering after SQL timeouts.
 * Fix a problem where stopping Zotonic could take a long time. Fixes #2670

Rob van den Bogaard (1):

 * Merge pull request #2659 from zotonic/sitemap-0x

