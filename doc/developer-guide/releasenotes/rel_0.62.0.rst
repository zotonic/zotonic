.. _rel-0.62.0:

Release 0.62.0
==============

Welcome to Zotonic 0.62.0, released on July 7, 2021.

This is a maintenance release.

Main changes are:

 * Fixed an issue where lib .tpl files where not search inside the lib folder, the behavior is now
   corrected and the same as in the 1.x release
 * Fixed a problem where large file uploads could give a timeout
 * Fixed a problem where the progress bar was not shown during file uploads
 * Fixed a problem where SEO sitemap routines could crash on long URLs
 * Ensure that the default SEO canonical and shortlink URLs are absolute URLs

Incompatibilities:

* If you generated lib files using template files (.tpl) then ensure that those template files are
  present in the lib directory.


Commits since 0.61.0
--------------------

Marc Worrell (7):

 * Only update seo sitemap on insert/update actions.
 * Fix a problem where the module indexer was not ready before the sitetests could start.
 * Find lib .tpl files inside lib. This makes the location of lib .tpl files consistent with 1.x and also gives less surprises where those files can be found.
 * Upgrade mochiweb
 * Use abs uri for canonical and shortlink
 * Make SEO sitemap loc 2000 chars long to accomodate longer URLs
 * Fix a problem where the progress was not shown during file uploads. Fix a problem where a file upload could show a timeout alert.
