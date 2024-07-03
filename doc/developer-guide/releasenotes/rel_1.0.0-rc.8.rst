Release 1.0.0-rc.8
==================

Released on 2022-09-13.

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

Commits since 1.0.0-rc.7
------------------------

Dorien (1):

 * Mod_admin: Add disconnect btn for page blocks (#3040)

MM Zeeman (2):

 * Fixed the documenation, and refactored the code to make it more readable (#3021)
 * Return not_found instead of no_session. Fixes #3057 (#3059)

Maas-Maarten Zeeman (2):

 * mod_acl_user_groups: Also log the kind of acl rule which is published
 * mod_wires: Prevent creation of global variables in zotonic-wired

Marc Worrell (81):

 * core: add z_datetime:next_month/2 and z_datetime:prev_month/2. (#3013)
 * core/oauth2: remove compile warning
 * Remove OTP 22 from the CI test matrix (#3024)
 * mod_search: adapt quick search presentation css (#3023)
 * core: call middlware 'request' notification after cookies has been parsed (#3028)
 * mod_seo_sitemap: update sitemap on pivot, remove prio 0 resources from urlsets (#3030)
 * core: add m_config:set_default_value/4
 * core: append name~<group> to make unique radio button groups with the same name. (#3031)
 * docs: use test topic in examples.
 * Fix an issue where rebar3 app_discovery could take a long time if there are many directories.
 * core: fix a problem where the split of an email did not work if the email address did not have a domain. (#3033)
 * core: fix a problem with splitting email addresses containing extended utf8 characters. (#3035)
 * doc: add 0.67.0 release notes
 * mod_admin: show estimated row count. (#3042)
 * core: define min lookahead for searches (for correct pagination) (#3044)
 * mod_l10n: Turkey is now called Türkiye (#3003)
 * Rename Turkey to Türkiye in country.pot.
 * mod_admin: fix a problem where the multi-lingual title of a page was lost on duplication. (#3047)
 * mod_base: make next/prev pager is page count is undefined. (#3050)
 * Upgrade zotonic_ssl to 1.3.0.  This fixes an issue with LibreSSL where a self-signed key could not be generated. (#3051)
 * mod_base: fix a problem where controller_static_pages could not handle unicode filenames (#3054)
 * mod_admin: Fix a problem where the pagelen was reset if the sort was changed. (#3062)
 * core: add config 'filewatcher_terminal_notifier' to optionally disable terminal messages on file updates. (#3064)
 * doc: add release notes of 0.68.0
 * Update rel_0.68.0.rst
 * mod_translation: include hreflang link to current translation (#3006)
 * core: on rsc update, as sudo, do not allow to add edges. (#3067)
 * core: add functions to add/sub N second/minute/hour/week/month/year to datetimes. (#3068)
 * mod_base: fix spec of z_list_of_ids_filter:filter functions. (#3073)
 * mod_oauth2: support client credentials grant type, misc fixes/changes (#3070)
 * mod_oauth2: better error texts.
 * mod_authentication: service error cancel cleanup
 * mod_oauth2: added support for new error 'unexpected_user'
 * core: filter identities returned via the m.identity api. (#3076)
 * Dockerfile: fix for untrusted keys.
 * smtp: do not embed images > 1MB (#3083)
 * core: fix an issue where the search_result page count could be off by one for certain searches. (#3085)
 * core: fix a problem where a search could result in illegal SQL
 * Merge pull request #3086 from zotonic/sql-append-publish
 * GH Actions: upgrade ubuntu version for Docs action
 * Merge pull request #3090 from zotonic/gh-action-docs-ubuntu-upgrade
 * core: add option to exclude categories from the SEO indexing
 * Merge pull request #3088 from zotonic/l10n_master
 * Upgrade jquery-ui to 1.13.2
 * Merge pull request #3093 from zotonic/jquery-ui-1.13
 * Fixes from review by @robvandenbogaard
 * Merge pull request #3091 from zotonic/seo-noindex-cat
 * mod_base: add option to controller_template to set the http status code (#3095)
 * mod_microsoft: change default scope, add comments. (#3100)
 * core: add z_context:site_url/2 (#3099)
 * mod_base: add csp_nonce to standard dispatcher args. (#3102)
 * facebook/linkedin/ms: On connected services show an x in the disconnect button
 * mod_menu: filter menu_is_visible now also checks for exists. (#3105)
 * mod_translation: sanitize URI for controller_set_language (#3112)
 * core: add timer on pw checks against timing attacks for finding valid usernames (#3098)
 * mod_video: escape filename for ffmpeg preview cmd (#3115)
 * mod_base: set the CSP for files to: default-src 'none'; sandbox (#3106)
 * doc: add 0.69.0 release notes.
 * mod_admin_identity: escape identity key
 * mod_authentication: ensure that logoff redirect url is sanitized. (#3116)
 * Upgrade template_compiler to 2.4.0
 * mod_search: add array support to the search facets (#3113)
 * core: change lookahead limit of searches for better pager display.
 * Add crowdin to readme thanks
 * Update links in readme
 * Remove unused macro, update copyright
 * mod_base: make the ... in the page clickable. (#3124)
 * core: adapt markdown for utf8 and modern string functions. (#3126)
 * mod_editor_tinymce: add zanchor plugin (#3127)
 * mod_editor_tinymce: Restrict height of tinymce edit area.
 * zotonic_core: fix for markdown conversion on Unicode strings with code points outside Ascii (#3128)
 * zotonic_core: add page urls to the model/rsc/get/id result. (#3129)

Michiel Klønhammer (25):

 * New Crowdin updates (#3015)
 * New translations zotonic.pot (Portuguese, Brazilian) (#3026)
 * New Crowdin updates (#3039)
 * New Crowdin updates (#3043)
 * New Crowdin updates (#3046)
 * New Crowdin updates (#3048)
 * New Crowdin updates (#3069)
 * New Crowdin updates (#3075)
 * New Crowdin updates (#3077)
 * New Crowdin updates (#3078)
 * New Crowdin updates (#3079)
 * New translations zotonic.pot (Russian) (#3080)
 * New Crowdin updates (#3081)
 * New Crowdin updates (#3084)
 * New translations zotonic.pot (Russian)
 * New Crowdin updates (#3101)
 * Update mod_auth2fa.erl
 * Merge branch 'master' of https://github.com/zotonic/zotonic
 * Update mod_auth2fa.erl
 * Update _auth2fa_user_actions.tpl
 * New Crowdin updates (#3107)
 * New Crowdin updates (#3110)
 * New translations zotonic.pot (Russian) (#3111)
 * New translations zotonic.pot (Russian) (#3119)
 * New Crowdin updates (#3121)
