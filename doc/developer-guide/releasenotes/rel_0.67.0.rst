.. _rel-0.67.0:

Release 0.67.0
==============

Welcome to Zotonic 0.67.0, released on July 5, 2022.

This is a maintenance release.

Main changes are:

 * Fix an issue with the sendfile repository being removed from GitHub
 * Better next/prev month calculation

Commits since 0.66.0
--------------------

Marc Worrell (5):

 * core: add z_datetime:next_month/2 and z_datetime:prev_month/2. (#3014)
 * core: fix an issue where the email recipient was not properly escaped when an override was set. (#3018)
 * mod_seo_sitemap: update sitemap on pivot, remove prio 0 resources from urlsets (0.x) (#3029)
 * Update webzmachine, remove sendfile (#3034)
 * Remove sendfile from .app.src
