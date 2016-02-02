.. _rel-0.13.4:

Release 0.13.4
==============

Welcome Zotonic 0.13.4, released on September 16, 2015.

This is a maintenance release of Zotonic 0.13

Main changes are:

 * fixed an issue where mod_survey answers could be shown without escaping
 * fixes for admin css and template issues
 * corrected handling of unsafe (escaped) characters in email addresses
 * easier access rule definitions by special handling of 'meta' category
 * fixed an issue where mod_video would crash if there was an empty ffmpeg configuration
 * fixed an issue where the last row of an imported CSV file could be reversed


Commits since 0.13.3
--------------------

There were 34 commits since release 0.13.3.

Big thanks to all people contributing to Zotonic!


Git shortlog
............

Arthur Clemens (17):

    *  Pass property is_dependent in datamodel; show status in edit page even if protected
    *  Apply tab style to all tabs within widgets
    *  Typo
    *  mod_admin: use relative date in event info
    *  docs: exemplify unique ids
    *  mod_admin: allow other modules to get result from connect dialog by passing option 'delegate'
    *  mod_admin: give first input field focus when switching tab
    *  mod_admin: improve layout of navigation bars
    *  mod_admin: improve layout of navigation bars: fix toolbar padding
    *  mod_admin: improve layout of navigation bars: improve some colors
    *  docs: add example of a postback action called from javascript
    *  mod_admin: add styling for justified tabs
    *  doc tweaks
    *  mod_admin: override style for code
    *  Merge remote-tracking branch 'origin/release-0.13.x' into release-0.13.x
    *  Revert "Merge remote-tracking branch 'origin/release-0.13.x' into release-0.13.x"
    *  mod_admin: override style for code: fix top bar

Marc Worrell (16):

    *  Merge pull request #1022 from millenaarg/release-0.13.x
    *  mod_import_csv: fix an issue where the last row is reversed iff there is no empty line at the end of the file.
    *  mod_backup: add option to make a database-only backup. Also make the backup list a live include.
    *  mod_backup: fix build by removing '&' from @doc
    *  core: fix a problem where email addresses couldn't have a ' (single quot) in them. Fixes #1023
    *  mod_video: use the default ffmpeg command if the configured ffmpeg command is empty.
    *  mod_video: use the default ffprobe and ffmpeg_preview command if the configured command is empty.
    *  New dependencies
    *  mod_acl_user_groups: ACL rules only apply to category 'meta' if either 'meta' or the system content group is mentioned in the ACL rule. Fixes #1030
    *  mod_l10n: added Kosovo (code xk) to the country list.
    *  mod_acl_user_groups: add 'view-only' option. Fixes #1034
    *  mod_acl_user_groups: show error if access to the ACL rules view is denied.
    *  mod_acl_user_groups: remove 'not' that was left in for checking.
    *  core: notify #hierarchy_updated observers which ids have added and/or removed.
    *  mod_menu: show the category in menu-list items muted.
    *  mod_survey/mod_base: url escape google chart arguments; escape survey label names.

millenaarg (1):

    *  Added reference to scomps-script
