.. _rel-0.17.0:

Release 0.17.0
==============

Welcome to Zotonic 0.17.0, released on 6 June, 2016.

Main changes are:

* Added :issue:`1274`: SNI support on Erlang 18.3 and higher.
* Added :issue:`1284`: default ACL rules.
* Added :issue:`1240`, :issue:`1276` and :issue:`1283`: documentation for ACL
  user groups, task queue and Google Analytics.
* Added :issue:`1265` and :issue:`1268`: sanitise SVG uploads and link tags.
* Fixed :issue:`1285`: be less verbose when inserting ACL rules.
* Fixed :issue:`1272`: Vimeo embeds.
* Fixed :issue:`1271`: protected media security.
* Fixed :issue:`1207` by giving ``m_rsc.uri`` property precedence over generated
  URI.
* Fixed :issue:`1132` by setting ``Content-Security-Policy`` header.

Commits since 0.16.0
--------------------

There were 54 commits since release 0.16.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

Arjan Scherpenisse (11):
      * scripts: Fix prepare-release.sh
      * doc: Fix version nr; add missing rst files; update installation requirements
      * doc: Update sidebar, update installation instructions
      * admin: Show collaboration groups in content dropdown on overview
      * mod_admin: Show content groups + page size filters on media overview
      * zotonic_status: Show site name next to URL
      * mod_acl_user_groups: Fix typo in edit check on the collab group itself
      * mod_email_dkim: Add DKIM signing of outgoing emails
      * mod_base: Add 'without' filter
      * doc: Add filter_without to filter toc
      * mod_acl_user_groups: Fix permission check for adding members/managers to group

Arthur Clemens (1):
      * core: use short notation to include the header

David de Boer (14):
      * deps: Lock erlang_localtime
      * doc: Fix absolute/relative URL terminology
      * mod_admin_identity: Fix verification e-mail URL
      * doc: Update release branch name
      * Add 0.16.0 release notes
      * Clean up README and fix dead links
      * doc: Document Google Analytics
      * doc: Document media caption
      * base: Fix figcaption tag
      * mod_acl_user_groups: Insert default ACL rules (see #1131)
      * doc: Document the task queue
      * mod_acl_user_groups: Be less verbose when editing and publishing ACL rules
      * doc: Document mod_acl_user_groups
      * doc: Fix typos

Maas-Maarten Zeeman (2):
      * build: Locked new mochiweb in order to support SSL on IE9 and 10 on OTP 18+
      * core: Move ssl listeners to the core and support SNI.

Marc Worrell (26):
      * mod_signup: use foldr for signup_form_fields, let higher prio modules win.
      * deps: switch to original erlang_localtime from dmitryme. Issue #1036
      * mod_acl_user_groups: tune access permissions for collaboration groups.     All collab group members can view the collab group.     If someone can update/link/delete a collab group, then that user can do the same on the collab group content.     Rename the config collab_group_edit to collab_group_update.
      * mod_acl_user_groups: members of a collaboration group can view each other.
      * core: add support for '--' operator, extend support for '++' operator.
      * core: add sanitization on the contents of uploaded SVG files. Issue #1265
      * Lock new dispatch_compiler. Fixes #1261
      * core: in m_rsc, always let the uri property take presendence above the local 'id' rule for non-informatonal uri generation. Fixes #1207
      * core: fix a problem where a file could be downloaded iff the file is not stored via a filestore. Fixes #1271
      * mod_video_embed: fix a problem where Vimeo embeds did not show a preview images. Fixes #1272
      * mod_admin: in depiction upload dialog, enable the 'upload' tab by default.
      * core: in html sanitize, add 'noopener noreferrer' to <a/> tags with a 'target' attribute.
      * mod_video_embed: better handling of 404 when fetchin Vimeo thumbnail.
      * core: added extra SVG sanitization.
      * Lock new z_stdlib for SVG sanitization.     Issue #1265
      * mod_survey: also mail all uploaded files to the 'survey_email' email address.
      * mod_admin: set X-Frame-Options: SAMEORIGIN header for admin pages. Issue #1132
      * mod_admin: fix is_authorized/2 in controller_admin_edit
      * mod_base: set CSP sandbox header if an user uploaded file is served with controller_file. Issue #1132
      * mod_acl_user_group: add option to move resources to collaboration groups the user is not member of.
      * mod_admin: allow to select multiple connection in the connection-find dialog.     Use the option 'autoclose' to change the text of the close button to 'cancel'.
      * mod_acl_user_groups: better overview of rules. Use dialog for editing rules.
      * mod_acl_user_groups: in acl rule edit, clear collaboration group search when group is selected.
      * mod_acl_user_groups: fix filtering on content groups when searching.     This fixes a problem when an user is allowed to see all or specific collaboration groups via the ACL rules.
      * mod_survey: fix layout of admin options.
      * mod_survey: in the emails, also show any 'injected' fields.     This allows the template to dynamically inject some answers without corresponding questions.*
