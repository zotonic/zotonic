Release 1.0.0-rc.15
===================

Released on 2023-01-30.

This is a release candidate for Zotonic 1.0.

The code is stable and used in production. Some features will be added before the 1.0 release.

For the issues and changes to be added for the final 1.0.0 release, see:

https://github.com/zotonic/zotonic/milestone/3

The 1.0.0 release is now expected end of February, 2023.

Updating
--------

After updating Zotonic, also update Cotonic with:

    make update-cotonic

This release needs the newest (master) version of Cotonic.

For the 1.0.0 we will start tagging the Cotonic versions.

Erlang/OTP Version
------------------

This release needs Erlang/OTP 23 or later.

Commits since 1.0.0-rc.14
-------------------------


Colin de Roos (1):

 * Manage system dependencies with Nix (#3253)

David de Boer (2):

 * core: Force ipv6
 * Revert "core: Force ipv6"

Dorien Drees (2):

 * Add nocatselect to the dialog (#3273)
 * Amazing changes for admin frontend edit (#3272)

Maas-Maarten Zeeman (3):

 * Upgrade exometer to 1.6.1. There was a problem with restarting reporters. (#3247)
 * docs: Add documentation for the autologon expire setting (#3279)
 * core: Fix expiry time of autologon cookie

Marc Worrell (34):

 * mod_mailinglist: fix an issue where not all mailinglists were shown on the mailing page. (#3230)
 * core: Fix an issue where the pivot could loop on some events (#3231)
 * core: in m_edge be more flexible about subject/object (#3229)
 * mod_admin: fix a problem where scheduled re-pivots disabled the button 'Rebuild search indices' (#3233)
 * mod_admin_modules: show version of modules in modules overview (#3234)
 * mod_admin_modules: extra escape
 * Upgrade webdavfilez
 * mod_admin: fix a problem where tinymce was not initialized (#3239)
 * mod_translation: add translate filter (#3238)
 * mod_search: robust against errors in queries (#3240)
 * mod_survey: fix a problem with the survey_is_submit filter.
 * mod_backup: fix revision diff view (#3244)
 * core: Fix for importing resources where unique name was used to find resources (#3246)
 * New pot - minor text changes
 * mod_video: fix a problem where preview generation would crash on audio-only videos. (#3250)
 * docs: try to fix version of alabaster (default theme of sphinx) (#3252)
 * mod_translation: better SEO handling of x-default language (#3256)
 * mod_wires: remove action notify (#3255)
 * mod_translation: remove title attribute from hreflang link tags (#3260)
 * core: after module schema error, flush caches (#3258)
 * core: in z_csv_writer, do not escape all fields starting with a digit (#3263)
 * core: use type file:filename_all in z_csv_writer (#3266)
 * core: in m_search only add qargs if the payload is empty (#3265)
 * mod_survey: in survey results editor show user and allow to link or create new person (#3264)
 * mod_admin: misc fixes for the connect/disconnect dialog (#3268)
 * mod_cookie_consent: new modules for asking consent to place cookies (#3259)
 * Upgrade zotonic_stdlib and tls_certificate_check
 * New pot
 * mod_admin: let the connect_created_me also influence the connect-find cg select (#3270)
 * mod_search: add search term to filter on visible_for property (#3278)
 * mod_wires: use form reset from zotonic-wired.js (#3277)
 * mod_survey: option to add survey participants to mailing list (#3276)
 * mod_editor_tinymce: 'latest' should be 'newest'
 * New pot

Michiel Klønhammer (4):

 * New translations zotonic.pot (Russian) (#3242)
 * New Crowdin updates (#3251)
 * New Crowdin updates (#3271)
 * New Crowdin updates (#3282)
