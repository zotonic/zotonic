.. _rel-0.43.0:

Release 0.43.0
==============

Welcome to Zotonic 0.43.0, released on 19 December, 2018.

Main changes are:

 * Allowed files in mod_acl_user_groups are now configurable
 * Security fixes for reflected XSS in the admin and skel/blog/archives.tpl
 * Hardened HTTP headers for securing Zotonic sessions and requests
 * mod_twitter now uses polling for fetching tweets, stopped using deprecated streaming API


Security Advisory
-----------------

If you have a blog site derived from the skel/blog then replace the
archives.tpl file in your site with the one provided in priv/skel/blog/archives.tpl


Compatibility
-------------

If you include a page of your site inside a frame on another site, then set the ``allow_frame``
option on the affected dispatch rule.


Commits since 0.42.0
--------------------

David de Boer (1):

     * docker: Build on Erlang 19.3 (#1950)

Maas-Maarten Zeeman (1):

     * Support binary data over websockets. Fixes #1953

Marc Worrell (4):

     * mod_editor_tinymce: fix a problem where zmedia stopped parsing if a non zmedia image was encountered.
     * Fix a problem with reusing ids for tinymce editors.
     * mod_twitter: stream api has been removed, add poller instead. (#1955)
     * Make acceptable mime types configurable per user group (#1956)

Michel Rijnders (3):

     * Remove unnecessary call to internal function (#1947)
     * Exclude node_modules from the file watcher (#1948)
     * Remove empty check (#1957)
