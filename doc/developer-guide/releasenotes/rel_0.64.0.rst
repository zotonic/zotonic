.. _rel-0.64.0:

Release 0.64.0
==============

Welcome to Zotonic 0.64.0, released on March 4, 2022.

This is a maintenance release.

Main changes are:

 * Add Instagram Javascript to the sanitizer whitelist.
 * Show username and last logon date in the admin on the user's page and in the user lists.
 * Better support for additional URLs in the SEO sitemap table.


Commits since 0.63.0
--------------------

Marc Worrell (7):

 * Fix an issue where the allowed predicates of a subject did not change when its category was updated. Fixes #2791
 * seo_sitemap: fix a problem where resources were not final-checked for inclusion in the sitemap.
 * Allow www.instagram.com/embed.js in the sanitizer.
 * In the admin show information about username and last logon.
 * mod_filestore: add simple test function to test the filestore connection.
 * seo_sitemap: add m_seo_sitemap:delete_before/3. (0.x)
 * seo_sitemap: add index on modified. Add sql timeouts.
 * Fix a problem where heart restarted Zotonic after a zotonic stop command. Fixes #2715

Rob van den Bogaard (2):

 * Merge pull request #2874 from zotonic/0x-m_seo_sitemap-delete_before
 * seo_sitemap: add notification at sitemap rebuild for custom indexing (#2894)
