Release 1.0.0-rc.11
===================

Released on 2022-11-11.

This is a release candidate for Zotonic 1.0.

The code is stable and used in production. Some features will be added before the 1.0 release.

For the issues and changes to be added for the final 1.0.0 release, see:

https://github.com/zotonic/zotonic/milestone/3

The 1.0.0 release is expected in Q4 2022.


Updating
--------

After updating Zotonic, also update Cotonic with:

    make update-cotonic

This release needs the newest (master) version of Cotonic.

For the 1.0.0 we will start tagging the Cotonic versions.

Erlang/OTP Version
------------------

This release needs Erlang/OTP 23 or later.

Commits since 1.0.0-rc.10
-------------------------

Marc Worrell (12):

 * New pot
 * mod_admin_frontend: support linking to specific tab in edit page (#3165)
 * mod_seo: longer seo descriptions, noindex for q.page > 1 (#3166)
 * core: ensure metadata is set on spawns (#3168)
 * mod_filestore: upgrade webdavfiles, fix upload with undefined mime (#3169)
 * mod_search: support OR query for language search term (#3170)
 * core: in search, allow pagelen to be undefined. Fixes #3171 (#3173)
 * core: find site path for setup without umbrella apps (#3174)
 * mod_video_embed: handle embed of youtube embed video links (#3175)
 * mod_base: fix a problem with filter toc where a '>' was shown before the texts (#3176)
 * mod_filestore: truncate remote filenames (#3177)
 * mod_seo_sitemap: exclude resources with a redirect from sitemap. Fixes #3157 (#3178)

Michiel Klønhammer (1):

 * New Crowdin updates (#3163)
