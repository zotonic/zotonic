.. _rel-0.69.0:

Release 0.69.0
==============

Welcome to Zotonic 0.69.0, released on August 30, 2022.

This is a maintenance release.

Main changes are:

 * Security fixes by extra sanitization of redirect URLs in authentication and language switching
 * Fix an information leak about possible usernames by adding more constant time password checks, even if there is no valid user.
 * Update of jquery ui to 1.13.
 * Show 2FA status in the user overview in the admin.
 * Remove the APIs /api/base/persistent_get and /api/base/persistent_put.
 * Option to add a SEO noindex to all pages of a certain category.

Incompatible changes
--------------------

The persistent APIs are removed, as they could be used to store information on the server by anonymous users.

New configuration key
---------------------

With the configuration key ``site.email_images_noembed`` it is possible to disable the embedding of
images in emails. If this key is set to true value then the images will be URLs and downloaded from
the webserver when the email is viewed.

This results in smaller emails, but the images might not be viewed by the recipient (as some email
agents don't show image by default).

Commits since 0.68.0
--------------------

Colin de Roos (1):

 * Check for empty search string (#3074)

Marc Worrell (34):

 * mod_seo: add all available hreflang and x-default links to head. (#3066)
 * Set Docker files to Erlang 21.3
 * Dockerfile: update keys
 * Docker: add oidcc for docker images.
 * docs: add note in search.rst about difference in syntax between tpl and stored query
 * core: in emails, only embed images < 1MB. (#3082)
 * core: fix site.email_images_noembed check
 * core: fix an issue where the search_result page count was off by one.
 * core: fix a problem where a search could result in illegal SQL
 * Add 'delete username' button to change pw dialog (#3037)
 * core: add option to exclude categories from the SEO indexing.
 * Set docs ubuntu version to 20.04 - as 18.04 is being deprecated
 * GH Action docs: upgrade ubuntu and python
 * Add translations
 * Update po files
 * mod_base: upgrade jquery-ui to 1.13.1
 * Merge pull request #3092 from zotonic/jquery-ui-1.13-0x
 * Merge pull request #3089 from zotonic/seo-noindex-cat-0x
 * Fix po files
 * mod_base: add option to controller_template to set the http status code (0.x) (#3094)
 * mod_menu: filter menu_is_visible now also checks for exists.
 * mod_base: remove persistent_get and persistent_set services. Fix #3103 (#3104)
 * core: add timer on pw checks against timing attacks for finding valid usernames (0.x) (#3097)
 * mod_video: escape filename for ffmpeg preview cmd (#3114)
 * mod_base: more restrictive CSP header in controller_file (0.x) (#3108)
 * Filter the redirect args of logon/logoff to ensure that it is a local redirect (#3096)
